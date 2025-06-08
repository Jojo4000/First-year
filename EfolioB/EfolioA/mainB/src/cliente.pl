% Declara que o predicado cliente/3 é dinâmico, permitindo assert/retract em tempo de execução
:- dynamic cliente/3.

% Permite que db_path/1 possa ser estendido em multiplos ficheiros
:- multifile db_path/1.
% Declara db_path/1 também como dinâmico para assertz posteriores
:- dynamic db_path/1.

% Aserção do caminho para o ficheiro database.pl
:- assertz(db_path('/home/jojo4000/LingPproj/EfolioA/mainA/_build/default/database.pl')).


% Persiste apenas os factos cliente/3 no database.pl, mantendo  os outros factos intactos
persist_cliente_db :-
    % Obtém o caminho do ficheiro a partir de db_path/1
    db_path(Path),
    % Abre o ficheiro em modo leitura
    open(Path, read, In),
    % Lê todos os factos exceto cliente/3, acumulando em Others
    read_all_facts(In, [], Others),
    close(In),
    % Abre o mesmo ficheiro em modo escrita (trunca o conteúdo existente)
    open(Path, write, Out),
    % Reescreve todos os factos originais não relacionados com cliente/3
    forall(member(F, Others), portray_clause(Out, F)),
    % Reescreve, em seguida, todos os factos cliente/3 atuais
    forall(cliente(ID,Nome,Distrito),
           portray_clause(Out, cliente(ID,Nome,Distrito))),
           close(Out).

% Lê recursivamente todos os termos 
read_all_facts(In, Acc, Others) :-
    % Lê o próximo termo do stream
    read(In, Term),
    ( Term == end_of_file ->
        % Se for fim-de-ficheiro, reverte acumulador para preservar ordem original
        reverse(Acc, Others);
        functor(Term, cliente, 3) ->
        % Se o termo for cliente/3, ignora-o (não adiciona ao acumulador)
        read_all_facts(In, Acc, Others);
        % Caso contrário, adiciona o termo ao acumulador e continua
        read_all_facts(In, [Term|Acc], Others)).





% Encontra todos os clientes registrados e imprime cada um 
listar_clientes :-
    % findall/3 coleta factos cliente/3 em lista L
    findall(cliente(ID,Nome,Distrito), cliente(ID,Nome,Distrito), L),
    % Para cada cliente em L, formata e exibe saída
    forall(member(cliente(ID,Nome,Distrito), L),
           format("~w: ~w (~w)~n", [ID, Nome, Distrito]) ).


% Adiciona um cliente novo se o ID não existir, caso contrário falha com erro
adicionar_cliente(ID, Nome, Distrito) :-
    ( cliente(ID,_,_) ->
        % Se já existe, imprime erro e falha
        format(user_error, "Erro: cliente ~w já existe~n", [ID]),
        fail;
        % Caso contrário, insere o novo cliente e persiste alterações
        assertz(cliente(ID, Nome, Distrito)),
        persist_cliente_db).

% Remove todos os factos cliente/3 que correspondam ao ID especificado
remover_cliente(ID) :-
    ( retractall(cliente(ID,_,_)) ->
        % Se removeu algum, persiste alterações
        persist_cliente_db;
        % Se não encontrou nenhum cliente com esse ID, exibe aviso
        format(user_error, "Aviso: cliente ~w não existe~n", [ID])).

% Atualiza apenas o campo Nome de um cliente existente
editar_cliente(ID, campo(nome), NovoNome) :-
    % Remove o cliente com campo Nome antigo
    retract(cliente(ID,_,Distrito)),
    % Insere de novo com o Nome atualizado
    assertz(cliente(ID, NovoNome, Distrito)),
    % Persiste alterações no ficheiro
    persist_cliente_db.


% Atualiza apenas o campo Distrito de um cliente existente
editar_cliente(ID, campo(distrito), NovoDistrito) :-
    % Remove o cliente com Distrito antigo
    retract(cliente(ID,Nome,_)),
    % Insere de novo com o Distrito atualizado
    assertz(cliente(ID, Nome, NovoDistrito)),
    % Persiste alterações
    persist_cliente_db.

% Exibe todos os clientes que residem num determinado distrito
pesquisar_cliente_distrito(Distrito) :-
    % Coleta factos de clientes com o mesmo Distrito
    findall(cliente(ID,Nome,Distrito), cliente(ID,Nome,Distrito), L),
    % Para cada membro, exibe formato "ID: Nome"
    forall(member(cliente(ID,Nome,_), L),
           format("~w: ~w~n", [ID, Nome]) ).


% Lista clientes cujo ValorTotal de algum orçamento excede ValorLimite
pesquisar_cliente_orc_superior(ValorLimite, Distrito) :-
    % Consulta o ficheiro database.pl para carregar factos orcamento/7
    db_path(Path),
    consult(Path),
    % Encontrar clientes e orçamentos que satisfaçam as condições
    findall(cliente(ID,Nome,Distrito),
            ( cliente(ID,Nome,Distrito),
              orcamento(_, ID, _, _, _, ValorTotal, _),
              ValorTotal > ValorLimite ), L),
    % Remove duplicados e ordena fazendo sort
    sort(L, Unique),
    % Exibe cada cliente filtrado
    forall(member(cliente(ID,Nome,_), Unique),
           format("~w: ~w~n", [ID, Nome])).
