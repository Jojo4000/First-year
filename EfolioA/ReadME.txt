Se fizer "dune build" certifique-se que existe uma database.pl dentro "/EfolioA/mainA/_build/default".

Para usar os comandos Java deve estar dentro da pasta mainB (cd mainB) e tem como comandos (que têm exemplos de IDs em frente):

java -cp bin OficinaApp listar_items
java -cp bin OficinaApp orcamento_items 1,2,4,7
java -cp bin OficinaApp orcamento_mecanico 1,2,3

Nota: Se os comandos Não tiverem IDs não vão correr!