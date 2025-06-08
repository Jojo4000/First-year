(* Define um tipo 'item' que representa um artigo com ID, nome, marca, tipo, custo, preço e quantidade *)
type item = {
    id    : int;
    nome  : string;
    marca : string;
    tipo  : string;
    custo : float;
    preco : float;
    qtd   : int;
}

(* Define um tipo 'servico' que representa um serviço mecânico com informações sobre ID, nome, categorias de peças, número de mecânicos, tempo estimado e preço base *)
type servico = {
  sid        : int;
  sname      : string;
  scats      : string list;
  n_mec      : int;
  tempo      : float;
  base_price : float;
}

(* Define um tipo 'desconto_marca' que associa uma marca a uma fração de desconto *)
type desconto_marca = {
  marca_desc : string;
  frac_desc  : float;
}

(* Define um tipo 'desconto_obra' que aplica um desconto baseado na comparação entre o tempo de serviço e um limiar *)
type desconto_obra =
  | Lt of float * float
  | Gt of float * float

(* Define um tipo 'mecanico' que representa um mecânico com ID, nome e custo por hora *)
type mecanico = {
  mid       : int;
  mname     : string;
  cost_hour : float;
}