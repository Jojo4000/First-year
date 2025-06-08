% Declara que o predicado item/7 é dinâmico, o que permite assert/retract em tempo de execução
:- dynamic item/7.

:- multifile db_path/1.
:- dynamic db_path/1.

% Define o caminho para o ficheiro database.pl 
:- assertz(db_path('/home/jojo4000/LingPproj/EfolioA/mainA/_build/default/database.pl')).


% Predicado principal para listar todos os itens 

listar_itens :-

    % findall/3: encontra todas as soluções do padrão item e coloca em Itens
    findall(item(ID,Nome,Marca,Tipo,Custo,PrecoVenda,Qtd),
            item(ID,Nome,Marca,Tipo,Custo,PrecoVenda,Qtd),
            Itens),
    % Verifica se a lista Itens está vazia
    ( Itens = [] ->
        % Caso vazia: exibe mensagem informativa
        writeln('Nenhum item encontrado.');   
        % Caso não vazia: percorre cada membro da lista e formata saída
        forall(member(item(ID,Nome,Marca,Tipo,Custo,PrecoVenda,Qtd), Itens), 
          format('~w;~w;~w;~w;~2f;~2f;~w~n', 
            [ID,Nome,Marca,Tipo,Custo,PrecoVenda,Qtd]))).


% Insere um novo item se o ID não existir, caso contrário dá erro

adicionar_item(ID,Nome,Marca,Tipo,Custo,PrecoVenda,Qtd) :-
    % Checa existência do item pelo ID
    ( item(ID,_,_,_,_,_,_) ->
        % Se existe: imprime mensagem de erro e falha
        format(user_error, "Erro: item ~w já existe~n", [ID]),
        fail;   % Se não existe: adiciona o facto e persiste alterações
        assertz(item(ID,Nome,Marca,Tipo,Custo,PrecoVenda,Qtd)),
        persist_db).

% Remove todos os factos item/7 que correspondam ao ID e persiste

remover_item(ID) :-
    % retractall/1 remove todos os matchings
    ( retractall(item(ID,_,_,_,_,_,_)) ->
        % Se houve remoção, persiste alterações em ficheiro
        persist_db;
        % Se nenhum facto removido, avisa usuário
        format(user_error, "Aviso: nenhum item com ID ~w~n", [ID])).


% Cada cláusula trata de um campo específico 
% Usam retract/1 para remover o facto antigo e assertz/1 para inserir o novo

atualizar_item(ID, campo(nome), NovoNome) :-
    retract(item(ID,_,Marca,Tipo,Custo,PrecoVenda,Qtd)),
    assertz(item(ID,NovoNome,Marca,Tipo,Custo,PrecoVenda,Qtd)),
    persist_db.

atualizar_item(ID, campo(marca), NovaMarca) :-
    retract(item(ID,Nome,_,Tipo,Custo,PrecoVenda,Qtd)),
    assertz(item(ID,Nome,NovaMarca,Tipo,Custo,PrecoVenda,Qtd)),
    persist_db.

atualizar_item(ID, campo(tipo), NovoTipo) :-
    retract(item(ID,Nome,Marca,_,Custo,PrecoVenda,Qtd)),
    assertz(item(ID,Nome,Marca,NovoTipo,Custo,PrecoVenda,Qtd)),
    persist_db.

atualizar_item(ID, campo(custo), NovoCusto) :-
    retract(item(ID,Nome,Marca,Tipo,_,PrecoVenda,Qtd)),
    assertz(item(ID,Nome,Marca,Tipo,NovoCusto,PrecoVenda,Qtd)),
    persist_db.

atualizar_item(ID, campo(preco), NovoPreco) :-
    retract(item(ID,Nome,Marca,Tipo,Custo,_,Qtd)),
    assertz(item(ID,Nome,Marca,Tipo,Custo,NovoPreco,Qtd)),
    persist_db.

atualizar_item(ID, campo(quantidade), NovaQtd) :-
    retract(item(ID,Nome,Marca,Tipo,Custo,PrecoVenda,_)),
    assertz(item(ID,Nome,Marca,Tipo,Custo,PrecoVenda,NovaQtd)),
    persist_db.


% Persiste alterações reescrevendo apenas os factos item
persist_db :-
    % Obtém caminho do ficheiro a partir de db_path
    db_path(Path),
    % Abre ficheiro em modo leitura
    open(Path, read, In),
    % Lê todos os factos (exceto item) usando o helper read_all_facts
    read_all_facts(In, [], Others),
    close(In),
    % Abre ficheiro em modo escrita, truncando conteúdo
    open(Path, write, Out),
    % Reescreve factos originais, ou seja, todos menos os items
    forall(member(Fact, Others), portray_clause(Out, Fact)),
    % Para cada facto item atual, escreve as linhas formatadas
    forall(item(ID,Nome,Marca,Tipo,Custo,PrecoVenda,Qtd),
           format(Out, "item(~w, '~w', '~w', '~w', ~2f, ~2f, ~w).~n",
                  [ID,Nome,Marca,Tipo,Custo,PrecoVenda,Qtd])),
                  close(Out).


% Lê recursivamente todos os termos do stream, acumulando factos não-item/7
read_all_facts(In, Acc, Others) :-
    read(In, Term),
    ( Term == end_of_file ->
        % Se fim de ficheiro, inverte acumulador para manter ordem original
        reverse(Acc, Others); 
        ( functor(Term, item, 7) ->
          % Se o termo é item/7, ignora adicionando mesmo Acc
          Acc1 = Acc;  
          % Caso contrário, inclui o termo no acumulador
          Acc1 = [Term|Acc]),
        % Continua leitura recursiva
        read_all_facts(In, Acc1, Others)).
