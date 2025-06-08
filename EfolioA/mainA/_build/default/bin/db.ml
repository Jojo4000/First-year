(* Importa definições de tipos item, servico e desconto_marca *)
open Types

(* Lê todas as linhas de um ficheiro para memória *)
let read_file (filename : string) : string list =
  (* Abre o ficheiro para leitura *)
  let chan = open_in filename in
  (* Função recursiva para ler todas as linhas *)
  let rec loop acc =
    (* Tenta ler uma linha e adiciona ao acumulador *)
    try loop (input_line chan :: acc)
    (* Se terminar o ficheiro, fecha-o e devolve as linhas *)
    with End_of_file -> close_in chan; List.rev acc in
  (* Inicia a leitura com lista vazia *)
  loop []

(* Extrai um item de uma linha em formato Prolog *)
let parse_item (line : string) : item option =
  (* Expressão regular para capturar campos do item *)
  let re = Str.regexp "item(\\([0-9]+\\), '\\([^']+\\)', '\\([^']+\\)', '\\([^']+\\)', \\([0-9.]+\\), \\([0-9.]+\\), \\([0-9]+\\))." in
  (* Verifica se a linha corresponde ao padrão *)
  if Str.string_match re line 0 then Some {
    (* Extrai e converte os campos *)
    id    = int_of_string   (Str.matched_group 1 line);
    nome  = Str.matched_group 2 line;
    marca = Str.matched_group 3 line;
    tipo  = Str.matched_group 4 line;
    custo = float_of_string (Str.matched_group 5 line);
    preco = float_of_string (Str.matched_group 6 line);
    qtd   = int_of_string   (Str.matched_group 7 line);
  } else None

(* Extrai um serviço de uma linha em formato Prolog *)
let parse_service (line : string) : servico option =
  (* Expressão regular para capturar campos do serviço *)
  let re = Str.regexp "servico(\\([0-9]+\\), '\\([^']+\\)', \\[\\([^]]*\\)\\], \\([0-9]+\\), \\([0-9.]+\\), \\([0-9.]+\\))." in
  (* Se a linha não corresponder, devolve None *)
  if not (Str.string_match re line 0) then None else
  (* Extrai campos individuais *)
  let id         = int_of_string   (Str.matched_group 1 line) in
  let name       = Str.matched_group 2 line in
  let cats_raw   = Str.matched_group 3 line in
  (* Processa as categorias para remover aspas *)
  let scats =
    String.split_on_char ',' cats_raw
    |> List.map String.trim
    |> List.map (fun s ->
         let len = String.length s in
         if len >= 2 && s.[0] = '\'' && s.[len-1] = '\'' then String.sub s 1 (len - 2) else s) in
  let n_mec      = int_of_string   (Str.matched_group 4 line) in
  let tempo      = float_of_string (Str.matched_group 5 line) in
  let base_price = float_of_string (Str.matched_group 6 line) in
  (* Devolve o serviço como record *)
  Some { sid        = id;
         sname      = name;
         scats      = scats;
         n_mec      = n_mec;
         tempo      = tempo;
         base_price = base_price }

(* Extrai desconto de marca de uma linha *)
let parse_discount_marca (line : string) : desconto_marca option =
  (* Expressão regular para o desconto *)
  let re = Str.regexp "desconto_marca('\\([^']+\\)', \\([0-9.]+\\))." in 
  (* Verifica correspondência e extrai campos *)
  if Str.string_match re line 0 then Some {
     marca_desc = Str.matched_group 1 line;
     frac_desc  = float_of_string (Str.matched_group 2 line);
   } else None

(* Formata um item como uma string delimitada por ponto e vírgula *)
let format_item (it : item) : string =
  Printf.sprintf "%d;%s;%s;%s;%.2f;%.2f;%d"
    it.id it.nome it.marca it.tipo it.custo it.preco it.qtd

(* Imprime lista de items, ordenados por tipo, marca e nome *)
let print_items (items : item list) : unit =
  items
  |> List.sort (fun a b ->
       match String.compare a.tipo b.tipo with
       | 0 -> (match String.compare a.marca b.marca with
               | 0 -> String.compare a.nome b.nome
               | c -> c)
       | c -> c)
  |> List.iter (fun it -> print_endline (format_item it))

(* Extrai um desconto de mão de obra de uma linha *)
let parse_discount_obra line : desconto_obra option =
  (* Expressão regular para desconto de tempo menor que limiar *)
  let re_lt = Str.regexp "desconto_mao_obra(<\\([0-9.]+\\), \\([0-9.]+\\))." in
  (* Expressão regular para desconto de tempo maior que limiar *)
  let re_gt = Str.regexp "desconto_mao_obra(>\\([0-9.]+\\), \\([0-9.]+\\))." in
  (* Verifica se é menor que o limiar *)
  if Str.string_match re_lt line 0 then
    let lim  = float_of_string (Str.matched_group 1 line) in
    let frac = float_of_string (Str.matched_group 2 line) in
    Some (Lt (lim, frac))
  (* Verifica se é maior que o limiar *)
  else if Str.string_match re_gt line 0 then
    let lim  = float_of_string (Str.matched_group 1 line) in
    let frac = float_of_string (Str.matched_group 2 line) in
    Some (Gt (lim, frac))
  else None

(* Extrai informação de um mecânico de uma linha *)
let parse_mecanico line : mecanico option =
  (* Expressão regular para capturar campos do mecânico *)
  let re = Str.regexp "mecanico(\\([0-9]+\\), \"\\([^']+\\)\", \\([0-9.]+\\))." in
  (* Verifica correspondência e extrai campos *)
  if Str.string_match re line 0 then Some {
    mid       = int_of_string   (Str.matched_group 1 line);
    mname     = Str.matched_group 2 line;
    cost_hour = float_of_string (Str.matched_group 3 line);
  } else None

(* Carrega todos os items da base de dados *)
let load_items () = read_file "database.pl" |> List.filter_map parse_item

(* Carrega todos os serviços da base de dados *)
let load_services () = read_file "database.pl" |> List.filter_map parse_service

(* Carrega todos os descontos de marca da base de dados *)
let load_desc_marca () = read_file "database.pl" |> List.filter_map parse_discount_marca

(* Carrega todos os descontos de mão de obra da base de dados *)
let load_desconto_mao_obra () = read_file "database.pl" |> List.filter_map parse_discount_obra

(* Carrega todos os mecânicos da base de dados *)
let load_mecanicos () = read_file "database.pl" |> List.filter_map parse_mecanico