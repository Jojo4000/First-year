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

(* Ponto de entrada do programa, análise dos argumentos da linha de comandos *)
let () = match Array.to_list Sys.argv with

(* Caso para listar todos os items *)
| _ :: "listar_items" :: _ ->
  (* Lê todas as linhas do ficheiro da base de dados *)
  let lines = read_file (locate_db_file ()) in
  (* Filtra e converte as linhas em items, depois imprime *)
  lines
  |> List.filter_map parse_item
  |> print_items

(* Caso para calcular orçamento de items para serviços *)
| _ :: "orcamento_items" :: svs :: _ ->
  (* Lê todas as linhas do ficheiro da base de dados *)
  let lines = read_file (locate_db_file ()) in
  (* Divide a lista de IDs recebidos, remove espaços e converte para inteiros *)
  let serv_ids = String.split_on_char ',' svs
    |> List.map String.trim
    |> List.map int_of_string in
  (* Filtra serviços válidos das linhas *)
  let services = lines
    |> List.filter_map parse_service in
  (* Filtra items válidos das linhas *)
  let items = lines
    |> List.filter_map parse_item in
  (* Filtra descontos de marca válidos das linhas *)
  let disc_m = lines
    |> List.filter_map parse_discount_marca in
  (* Cria uma tabela hash para guardar descontos por marca *)
  let tbl_m = Hashtbl.create (List.length disc_m) in
  (* Adiciona cada desconto à tabela hash *)
  List.iter (fun d ->
    Hashtbl.add tbl_m d.marca_desc d.frac_desc) disc_m;

  (* Para cada ID de serviço fornecido *)
  List.iter (fun sid ->
    (* Procura o serviço correspondente na lista de serviços *)
    match List.find_opt (fun s -> s.sid = sid) services with
    | None ->
        (* Se o serviço não for encontrado, imprime erro *)
        prerr_endline (Printf.sprintf "Serviço %d não encontrado" sid)
    | Some svc ->
        (* Seleciona os melhores items para o serviço *)
        select_best_items_for_service
          ~service:svc
          ~items
          ~disc_tbl:tbl_m
        (* Para cada item selecionado *)
        |> List.iter (fun it ->
          (* Obtém o desconto da marca ou 0.0 se não existir *)
          let d = Hashtbl.find_opt tbl_m it.marca |> Option.value ~default:0.0 in
          (* Calcula o lucro para o item com desconto *)
          let p = profit_of_item d it in
          (* Imprime os detalhes do item e os valores calculados *)
          Printf.printf "%d;%d;%s;%s;%s;%.2f;%.2f;%.2f;%.2f\n"
            it.id svc.sid it.nome it.marca it.tipo it.custo it.preco d p)) serv_ids

(* Caso para calcular custo de mão de obra para serviços *)
| _ :: "orcamento_mecanico" :: svs :: _ ->
  (* Lê todas as linhas do ficheiro da base de dados *)
  let lines = read_file (locate_db_file ()) in
  (* Divide a lista de IDs recebidos, remove espaços e converte para inteiros *)
  let serv_ids = String.split_on_char ',' svs
    |> List.map String.trim
    |> List.map int_of_string in
  (* Filtra serviços válidos das linhas *)
  let all_svcs = lines |> List.filter_map parse_service in
  (* Filtra mecânicos válidos das linhas *)
  let mech_tbl : mecanico list = lines |> List.filter_map parse_mecanico in
  (* Filtra descontos de obra válidos das linhas *)
  let ld_tbl : desconto_obra list = lines |> List.filter_map parse_discount_obra in

  (* Para cada ID de serviço fornecido *)
  List.iter (fun sid ->
    (* Procura o serviço correspondente na lista de serviços *)
    match List.find_opt (fun s -> s.sid = sid) all_svcs with
    | None ->
        (* Se o serviço não for encontrado, imprime erro *)
        prerr_endline (Printf.sprintf "Serviço %d não encontrado" sid)
    | Some svc ->
        (* Calcula o custo da mão de obra para o serviço *)
        let h, ch, b, disc, total =
          Logic.labor_cost
            ~service:svc
            ~mecs:mech_tbl
            ~desc_obras:ld_tbl in
        (* Imprime o resultado do cálculo de mão de obra *)
        Printf.printf "%d;%.2f;%.2f;%.2f;%.2f;%.2f\n"
          svc.sid h ch b disc total) serv_ids

(* Caso padrão, imprime instruções de uso *)
| _ ->
  Printf.printf "Uso:\n\
                \ mainA listar_items\n\
                \ mainA orcamento_items <id1,id2,...>\n\
                \ mainA orcamento_mecanico <id1,id2,...>\n"