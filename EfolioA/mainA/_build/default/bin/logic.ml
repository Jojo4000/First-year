(* Importa os tipos definidos no módulo Types *)
open Types

(* Função que calcula o lucro de um item após aplicar desconto *)
let profit_of_item discount_frac (it : item) : float =
  (* Calcula o preço final com desconto e subtrai o custo *)
  (it.preco *. (1.0 -. discount_frac)) -. it.custo

(* Função que seleciona os melhores items para um serviço específico *)
let select_best_items_for_service
    ~(service : servico)
    ~(items : item list)
    ~(disc_tbl : (string, float) Hashtbl.t)
  : item list =

  (* Para cada categoria requerida pelo serviço *)
  service.scats 
  |> List.filter_map (fun cat ->
       (* Filtra items que correspondem à categoria *)
       let candidates = List.filter (fun it -> it.tipo = cat) items in
       (* Calcula o lucro de cada candidato considerando o desconto *)
       let scored = List.map (fun it ->
         (* Tenta obter o desconto da marca, ou 0.0 se não existir *)
         let d = try Hashtbl.find disc_tbl it.marca with _ -> 0.0 in
         (* Associa o item ao seu lucro *)
         (it, profit_of_item d it)) candidates in
       (* Se não houver candidatos, retorna None *)
       match scored with
       | [] -> None
       (* Senão, seleciona o item com maior lucro *)
       | _ -> Some (fst (List.fold_left
          (fun (best_it, best_p) (it, p) ->
            if p > best_p then (it, p) else (best_it, best_p))
          (List.hd scored) scored)))

(* Função que calcula o custo da mão de obra para um serviço *)
let labor_cost ~service ~mecs ~desc_obras =
  (* Obtém o número de horas necessárias para o serviço *)
  let hours = service.tempo in
  (* Determina o custo por hora mais barato entre os mecânicos *)
  let cost_per_hour =
    mecs
    |> List.map (fun m -> m.cost_hour)
    |> List.fold_left min max_float in
  (* Calcula o custo base: horas × custo hora × número de mecânicos *)
  let base = hours *. cost_per_hour *. float_of_int service.n_mec in
  (* Procura desconto aplicável com base no tempo de serviço *)
  let frac = desc_obras
    |> List.find_map (function
      | Lt (lim, f) when hours < lim -> Some f
      | Gt (lim, f) when hours > lim -> Some f
      | _ -> None)
    |> Option.value ~default:0.0 in
  (* Calcula o valor do desconto *)
  let discount = base *. frac in
  (* Calcula o custo total depois do desconto *)
  let total = base -. discount in
  (* Retorna uma tupla com detalhes do cálculo *)
  (hours, cost_per_hour, base, discount, total)
