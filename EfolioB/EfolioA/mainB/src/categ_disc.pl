% Declara multiplos predicados como dinâmicos: servico/6, item/7, desconto_marca/2 e desconto_mao_obra/2
:- dynamic servico/6, item/7, desconto_marca/2, desconto_mao_obra/2.

% Permite que o predicado db_path/1 seja definido em vários ficheiros
:- multifile db_path/1.
% Declara db_path/1 como dinâmico para permitir assertz posterior
:- dynamic db_path/1.

% Aserção do caminho absoluto para o ficheiro database.pl
:- assertz(db_path('/home/jojo4000/LingPproj/EfolioA/mainA/_build/default/database.pl')).


% Predicado principal que lê o database.pl e reescreve categorias e descontos
persist_cd_db :-
    % Obtém o caminho do ficheiro a partir de db_path/1
    db_path(Path),
    % Abre o ficheiro em modo leitura para filtrar factos
    open(Path, read, In),
    % Lê todos os factos exceto servico/6, item/7, desconto_marca/2 e desconto_mao_obra/2
    read_all_facts(In, [], Others),
    close(In),
    % Reabre o ficheiro em modo escrita, truncando conteúdo anterior
    open(Path, write, Out),
    % Reescreve todos os factos originais (não filtrados)
    forall(member(F, Others), portray_clause(Out, F)),
    % Reescreve factos servico/6 atuais, preservando sintaxe Prolog
    forall(servico(ID,Name,Cats,Nm,Tmp,Base),
           portray_clause(Out, servico(ID,Name,Cats,Nm,Tmp,Base))),
    % Reescreve factos desconto_marca/2 formatados com 2 casas decimais
    forall(desconto_marca(Brand,Fm),
           format(Out, "desconto_marca('~w', ~2f).~n", [Brand, Fm])),
    % Reescreve factos desconto_mao_obra/2 formatados com 2 casas decimais
    forall(desconto_mao_obra(Cond,Fm),
           format(Out, "desconto_mao_obra(~w, ~2f).~n", [Cond, Fm])),
    % Reescreve factos item/7 atuais, preservando sintaxe Prolog
    forall(item(ID,Nome,Marca,Tipo,Custo,PrecoVenda,Qtd),
           portray_clause(Out, item(ID,Nome,Marca,Tipo,Custo,PrecoVenda,Qtd))),
           close(Out).


% Lê term a term do stream In e acumula os termos não relacionados
% Ignora servico, item, desconto_marca e desconto_mao_obra
read_all_facts(In, Acc, Others) :-
    % Lê o próximo termo do stream
    read(In, Term),
    (Term == end_of_file ->
        % Se fim de ficheiro, inverte acumulador para manter ordem original
        reverse(Acc, Others) ; 
        ( functor(Term, servico, 6); 
        functor(Term, item, 7);
         functor(Term, desconto_marca, 2);
          functor(Term, desconto_mao_obra, 2)) ->
        % Se o termo for um destes factos, ignora e continua
        read_all_facts(In, Acc, Others)
    ;   % Caso contrário, adiciona o termo ao acumulador
        read_all_facts(In, [Term|Acc], Others)).


% lista vazia produz lista vazia
replace_in_list([], _, _, []).

% Percorre a lista original [X|Xs]
replace_in_list([X|Xs], Old, New, [Y|Ys]) :-
    (X == Old ->
        % Se X é igual a Old, substitui por New
        Y = New;
    % Caso contrário, mantém X
        Y = X),
    % Continua substituição recursiva no resto da lista
    replace_in_list(Xs, Old, New, Ys).


listar_categorias :-
    % Coleta categorias de items (4ª posição em item/7)
    findall(T, item(_,_,_,T,_,_,_), Ts),
    % Coleta categorias de serviços (lista Cats em servico/6)
    findall(C, (servico(_,_,Cats,_,_,_), member(C, Cats)), Cs),
    % Concatena ambas as listas e ordena removendo duplicados
    append(Ts, Cs, All), sort(All, Unique),
    % Imprime cada categoria única
    forall(member(U, Unique), writeln(U)).

editar_categoria(Antiga, Nova) :-
    % Atualiza itens: encontra todos que usam a categoria Antiga
    findall(ID-N-M-Cu-P-Q,
            item(ID, N, M, Antiga, Cu, P, Q), ToItems),
    % Para cada item encontrado, retira e insere com categoria Nova
    forall(member(ID-N-M-Cu-P-Q, ToItems),
      (retract(item(ID,N,M,Antiga,Cu,P,Q)),
        assertz(item(ID,N,M,Nova,Cu,P,Q)) )),
    % Atualiza serviços: encontra todos que têm Antiga em Cats
    findall(ID-Name-Cats-Nm-Tmp-Base,
            (servico(ID,Name,Cats,Nm,Tmp,Base), member(Antiga, Cats)), ToServ),
    % Para cada serviço, retira, substitui em Cats e insere de novo
    forall(member(ID-Name-Cats-Nm-Tmp-Base, ToServ),
      (  retract(servico(ID,Name,Cats,Nm,Tmp,Base)),
        replace_in_list(Cats, Antiga, Nova, NewCats),
        assertz(servico(ID,Name,NewCats,Nm,Tmp,Base)) )),
    % Persiste alterações no ficheiro
    persist_cd_db.

remover_categoria(Cat) :-
    % Remove todos os itens com categoria igual a Cat
    retractall(item(_,_,_,Cat,_,_,_)),
    % Encontra IDs dos serviços que usam Cat
    findall(ID, (servico(ID,_,Cats,_,_,_), member(Cat, Cats)), SIDs),
    % Remove cada serviço inteiro caso contenha Cat
    forall(member(ID, SIDs), retractall(servico(ID,_,_,_,_,_))),
    % Persiste alterações
    persist_cd_db.



% listar_desconto_marca/0: imprime cada desconto de marca formatado
listar_desconto_marca :-
    forall(desconto_marca(B, F), format('~w: ~2f~n', [B, F]) ).

% adicionar_desconto_marca/2: acresce desconto se não existir
adicionar_desconto_marca(B, F) :-
    ( desconto_marca(B, _) ->
        format(user_error, 'Erro: desconto para marca ~w já existe~n', [B]), fail
    ; assertz(desconto_marca(B, F)), persist_cd_db ).

% editar_desconto_marca/2: remove e insere novo valor
editar_desconto_marca(B, F) :-
    retractall(desconto_marca(B, _)),
    assertz(desconto_marca(B, F)),
    persist_cd_db.

% remover_desconto_marca/1: retira todos os factos para a marca B
remover_desconto_marca(B) :-
    retractall(desconto_marca(B, _)),
    persist_cd_db.

% listar_desconto_mao_obra/0: imprime descontos de mao de obra
listar_desconto_mao_obra :-
    forall(desconto_mao_obra(C, F), format('~w: ~2f~n', [C, F]) ).

% adicionar_desconto_mao_obra/2: acresce se não existir
adicionar_desconto_mao_obra(C, F) :-
    ( desconto_mao_obra(C, _) ->
        format(user_error, 'Erro: desconto mão de obra ~w já existe~n', [C]), fail
    ; assertz(desconto_mao_obra(C, F)), persist_cd_db ).

% editar_desconto_mao_obra/2: atualiza valor existente
editar_desconto_mao_obra(C, F) :-
    retractall(desconto_mao_obra(C, _)),
    assertz(desconto_mao_obra(C, F)),
    persist_cd_db.

% remover_desconto_mao_obra/1: retira todos os factos correspondentes
remover_desconto_mao_obra(C) :-
    retractall(desconto_mao_obra(C, _)),
    persist_cd_db.
