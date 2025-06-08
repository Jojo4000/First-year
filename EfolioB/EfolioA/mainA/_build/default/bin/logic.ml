(* Importa os tipos definidos no módulo Types *)
open Types

(* Função que calcula o lucro de um item após aplicar desconto *)
let profit_of_item discount_frac (it : item) : float =
  (* Calcula o preço final com desconto e subtrai o custo *)
  (it.preco *. (1.0 -. discount_frac)) -. it.custo

(* formata a linha de output para um item+serviço *)
let format_item_for_service
    ~(item     : item)
    ~(service  : servico)
    ~(disc_tbl : (string,float) Hashtbl.t)
  : string =
  let d = Hashtbl.find_opt disc_tbl item.marca |> Option.value ~default:0.0 in
  let p = profit_of_item d item in
  Printf.sprintf
    "%d;%d;%s;%s;%s;%.2f;%.2f;%.2f;%.2f"
    item.id service.sid
    item.nome item.marca item.tipo
    item.custo item.preco d p

(* selecciona, por cada categoria do serviço, o item com maior lucro *)
let select_best_items_for_service
    ~(service  : servico)
    ~(items    : item list)
    ~(disc_tbl : (string,float) Hashtbl.t)
  : item list =
  service.scats
  |> List.filter_map (fun cat ->
       let candidates = List.filter (fun it -> it.tipo = cat) items in
       if candidates = [] then begin
         prerr_endline
           (Printf.sprintf
              "Aviso: serviço %d (‘%s’) – categoria “%s” sem artigos no catálogo"
              service.sid service.sname cat);
         None
       end else
         let scored = List.map (fun it ->
             let d = Hashtbl.find_opt disc_tbl it.marca |> Option.value ~default:0.0 in
             (it, profit_of_item d it)
           ) candidates in
         let best, _ =
           List.fold_left
             (fun (best_it, best_p) (it, p) ->
                if p > best_p then (it,p) else (best_it,best_p))
             (List.hd scored) scored
         in
         Some best)

(* Função que calcula o custo da mão de obra para um serviço *)
let labor_cost ~service ~mecs ~desc_obras =
  (* 1) Horas necessárias *)
  let hours = service.tempo in

  (* 2) Custo por hora mais barato; se não houver mecânicos, avisar e usar 0.0 *)
  let cost_per_hour =
    match mecs with
    | [] ->
        prerr_endline
          "Aviso: nenhum mecânico definido; custo hora considerado 0.0";
        0.0
    | _ ->
        mecs
        |> List.map (fun m -> m.cost_hour)
        |> List.fold_left min max_float
  in

  (* 3) Custo base = horas × custo hora × nº de mecânicos *)
  let base = hours *. cost_per_hour *. float_of_int service.n_mec in

  (* 4) Escolher fração de desconto: *)
  let frac =
    (* 4.1) sem regras carregadas? *)
    if desc_obras = [] then (
      prerr_endline
        (Printf.sprintf
           "Aviso: serviço %d (‘%s’) – nenhuma regra de desconto definida"
           service.sid service.sname);
      0.0
    ) else
      (* 4.2) filtrar só as regras que se aplicam *)
      let applicable =
        List.filter (function
          | Lt (lim, _) when hours < lim -> true
          | Gt (lim, _) when hours > lim -> true
          | _ -> false
        ) desc_obras
      in
      match applicable with
      | [] ->
          prerr_endline
            (Printf.sprintf
               "Aviso: serviço %d (‘%s’) – sem regra de desconto aplicável para %.2f horas"
               service.sid service.sname hours);
          0.0
      | [Lt (_, f)] | [Gt (_, f)] ->
          (* exatamente uma regra, usa-a *)
          f
      | _ ->
          (* várias regras aplicáveis: escolhe a maior fração e avisa *)
          let f_max =
            applicable
            |> List.map (function Lt (_, f) | Gt (_, f) -> f)
            |> List.fold_left max 0.0
          in
          prerr_endline
            (Printf.sprintf
               "Aviso: serviço %d (‘%s’) – múltiplas regras aplicáveis; \
                aplicado maior desconto %.2f"
               service.sid service.sname f_max);
          f_max in
  (* 5) Cálculo final *)
  let discount = base *. frac in
  let total    = base -. discount in
  (hours, cost_per_hour, base, discount, total)


(** a) Para cada serviço, lista as peças + desconto aplicado e valor do desconto *)
let orcamento_desconto_items
    ~(service : servico)
    ~(items   : item list)
    ~(disc_tbl: (string,float) Hashtbl.t)
  : (int * float * float) list =
  (* reutiliza select_best_items_for_service para obter só as peças *)
  select_best_items_for_service ~service ~items ~disc_tbl
  |> List.map (fun it ->
       let frac = Hashtbl.find_opt disc_tbl it.marca |> Option.value ~default:0.0 in
       let valor = it.preco *. frac in
       (it.id, frac *. 100.0, valor)
     )

(** b) Para cada serviço, devolve o preço fixo definido em base_price *)
let orcamento_preco_fixo
    ~(service : servico)
  : (int * float) =
  (service.sid, service.base_price)