% obter todos os clientes
listar_clientes(LC) :- 
    findall(cliente(ID,Nome,Distr), cliente(ID,Nome,Distr), LC).

% obter todos os orçamentos
listar_orcamentos(L) :- 
    findall(orcamento(I,C,D,S,It,V,P), orcamento(I,C,D,S,It,V,P), L).