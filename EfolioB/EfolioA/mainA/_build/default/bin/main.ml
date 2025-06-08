(* Importa o módulo que define os tipos usados *)
open Types

(* Importa o módulo que gere o acesso à base de dados *)
open Db

(* Importa o módulo que contém a lógica do programa *)
open Logic


(* Função para localizar o caminho do ficheiro da base de dados *)
let locate_db_file () : string =
  (* Obtém o caminho completo do executável *)
  let exe  = Sys.executable_name in
  (* Obtém o diretório onde está o executável *)
  let bin  = Filename.dirname exe in
  (* Sobe um nível na hierarquia de diretórios *)
  let root = Filename.dirname bin in
  (* Junta o caminho raiz ao nome do ficheiro da base de dados *)
  Filename.concat root "database.pl"

let item_re = Str.regexp
  "item(\\([0-9]+\\), '\\([^']+\\)', '\\([^']+\\)', '\\([^']+\\)', \\([0-9.]+\\), \\([0-9.]+\\), \\([0-9]+\\))."

(* Converte o resultado do parse_item em option, logando erros *)
let parse_item_opt line =
  (* 1. apenas tentamos converter se bater no padrão de item(...) *)
  if not (Str.string_match item_re line 0) then
    None
  else
    (* 2. chamamos o parse “forte” que valida campos e devolve Result *)
    match parse_item line with
    | Ok it   -> Some it
    | Error e ->
        prerr_endline ("[parse_item] "^e);
        None


(* Ponto de entrada do programa, análise dos argumentos da linha de comandos *)
let () = match Array.to_list Sys.argv with

  (* Caso para listar todos os items *)
  | _ :: "listar_items" :: _ ->
    (* Lê todas as linhas do ficheiro da base de dados *)
    let lines = read_file (locate_db_file ()) in
    (* Filtra e converte as linhas em items, depois imprime *)
    lines
    |> List.filter_map parse_item_opt
    |> print_items

(* Caso para calcular orçamento de items para serviços *)
  | _ :: "orcamento_items" :: svs :: _ ->
    let lines = read_file (locate_db_file ()) in

    (* ids de serviços vindos da linha-de-comandos *)
    let serv_ids = svs
      |> String.split_on_char ','
      |> List.map String.trim
      |> List.map int_of_string
    in

    (* carregar catálogos *)
    let services = lines |> List.filter_map parse_service in
    let items    = lines |> List.filter_map parse_item_opt in
    let disc_m   = lines |> List.filter_map parse_discount_marca in

    (* montar tabela de descontos *)
    let tbl_m = Hashtbl.create (List.length disc_m) in
    List.iter (fun d -> Hashtbl.add tbl_m d.marca_desc d.frac_desc) disc_m;

    (* para cada serviço pedido, selecionar + formatar *)
    List.iter (fun sid ->
      match List.find_opt (fun s -> s.sid = sid) services with
      | None ->
          prerr_endline (Printf.sprintf "Serviço %d não encontrado" sid)
      | Some svc ->
          select_best_items_for_service
            ~service:svc
            ~items
            ~disc_tbl:tbl_m
          |> List.iter (fun it ->
               (* formata cada linha via função no Logic.ml *)
               print_endline
                 (format_item_for_service
                    ~item:it
                    ~service:svc
                    ~disc_tbl:tbl_m))
    ) serv_ids

  (* ------------------------------------------------ *)
  (* 4a) Desconto em peças por serviço               *)
  | _ :: "orcamento_desconto_items" :: svs :: _ ->
    let lines   = read_file (locate_db_file ()) in
    let serv_ids = 
      svs |> String.split_on_char ',' |> List.map String.trim |> List.map int_of_string
    in
    let services = lines |> List.filter_map parse_service in
    let items    = lines |> List.filter_map parse_item_opt in
    let disc_m   = lines |> List.filter_map parse_discount_marca in
    let tbl_m    = Hashtbl.create (List.length disc_m) in
    List.iter (fun d -> Hashtbl.add tbl_m d.marca_desc d.frac_desc) disc_m;
    (* Para cada serviço pedido *)
    List.iter (fun sid ->
      match List.find_opt (fun s -> s.sid = sid) services with
      | None -> prerr_endline (Printf.sprintf "Serviço %d não encontrado" sid)
      | Some svc ->
        Logic.orcamento_desconto_items
        ~service:svc
        ~items
        ~disc_tbl:tbl_m
        |> List.iter (fun (id, pct, valor) ->
             (* imprime ID Item; Desconto (%); Valor do Desconto *)
             Printf.printf "%d;%.2f;%.2f\n" id pct valor
           )
    ) serv_ids

  (* 4b) Preço fixo por serviço *)
  | _ :: "orcamento_preco_fixo" :: svs :: _ ->
    let lines    = read_file (locate_db_file ()) in
    let serv_ids = 
      svs |> String.split_on_char ',' |> List.map String.trim |> List.map int_of_string
    in
    let services = lines |> List.filter_map parse_service in
    List.iter (fun sid ->
      match List.find_opt (fun s -> s.sid = sid) services with
      | None -> prerr_endline (Printf.sprintf "Serviço %d não encontrado" sid)
      | Some svc ->
        let id, valor = orcamento_preco_fixo ~service:svc in
        (* imprime ID Serviço; Valor *)
        Printf.printf "%d;%.2f\n" id valor
    ) serv_ids
       
(* Caso para calcular custo de mão de obra para serviços *)
  | _ :: "orcamento_mecanico" :: svs :: _ ->
      let lines    = read_file (locate_db_file ()) in
      let serv_ids = svs
        |> String.split_on_char ','
        |> List.map String.trim
        |> List.map int_of_string
      in

      let all_svcs = lines |> List.filter_map parse_service in
      let mech_tbl = lines |> List.filter_map parse_mecanico in
      let ld_tbl   = lines |> List.filter_map parse_discount_obra in

      List.iter (fun sid ->
        match List.find_opt (fun s -> s.sid = sid) all_svcs with
        | None ->
            prerr_endline (Printf.sprintf "Serviço %d não encontrado" sid)
        | Some svc ->
            let h, ch, base, disc, total =
              labor_cost
                ~service:svc
                ~mecs:mech_tbl
                ~desc_obras:ld_tbl
            in
            Printf.printf "%d;%.2f;%.2f;%.2f;%.2f;%.2f\n"
              svc.sid h ch base disc total
      ) serv_ids

    | _ ->
      Printf.printf
        "Uso:\n\
        \ mainA listar_items\n\
        \ mainA orcamento_items <id1,id2,...>\n\
        \ mainA orcamento_mecanico <id1,id2,...>\n"