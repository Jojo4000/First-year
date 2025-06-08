NOTAS :  **Se fizer "dune build", certifique-se de que existe uma database.pl dentro de "/EfolioA/mainA/_build/default".
         **Copiar fullpath da database.pl em "/EfolioB/EfolioA/mainA/_build/default/database.pl" e substituir em categ_disc.pl; cliente.pl; e inventory.pl
    em -> " :- assertz(db_path('/home/jojo4000/LingPproj/EfolioA/mainA/_build/default/database.pl'))."

Para usar os comandos Java deve estar dentro da pasta mainB (cd mainB) e tem como comandos (que têm exemplos de IDs em frente):

# Compila todas as classes Java, incluindo jpl.jar para integração Prolog
javac -cp "/usr/lib/swi-prolog/lib/jpl.jar" -d bin src/*.java

# ————— OCaml-driven commands (sem Prolog envolvido) —————

# Lista todos os serviços/peças definidos no módulo OCaml
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp listar_items

# Gera orçamento baseado na lista de IDs de serviço (1,2,5)
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp orcamento_items 1,2,5

# Calcula o custo de mão de obra para o serviço de ID 3
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp orcamento_mecanico 3

# Aplica descontos por peça para serviços cujos IDs estão na lista (1,8)
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp orcamento_desconto_items 1,8

# Calcula preço fixo para cada serviço na lista de IDs (1,8)
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp orcamento_preco_fixo 1,8


# ————— Prolog calls —————

Questão 1 (clientes & orçamentos):
# Lista todos os clientes registados em Prolog
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp listar_clientes

# Lista todos os orçamentos registados em Prolog
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp listar_orcamentos


Questão 2 (inventário):
# Lista todos os itens do inventário Prolog
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp listar_itens

# Adiciona um novo item com ID=34, nome=FiltroX, marca=MarcaX, tipo=TipoY, custo=10.0, venda=15.0, quantidade=20
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp adicionar_item 34,FiltroX,MarcaX,TipoY,10.0,15.0,20

# Remove o item com ID=34 do inventário
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp remover_item 34

# Atualiza o nome do item ID=8 para “NovoNome”
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp atualizar_item 8,campo\(nome\),NovoNome


Questão 3: Categorias & Descontos

# —— Categorias —— 

# Lista todas as categorias presentes em items e serviços
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp listar_categorias

# Renomeia a categoria “Oleos” para “Lubrificantes”
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp editar_categoria "Oleos,Lubrificantes"

# Remove a categoria “Kits Embreagem” (itens e serviços associados são eliminados)
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp remover_categoria "Kits Embreagem"

# —— Descontos de Marca —— 

# Lista todos os descontos de marca configurados
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp listar_desconto_marca

# Adiciona desconto de 20% (0.20) para a marca NGK
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp adicionar_desconto_marca "NGK,0.20"

# Edita o desconto da marca Bosch para 18% (0.18)
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp editar_desconto_marca "Bosch,0.18"

# Remove o desconto configurado para a marca Philips
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp remover_desconto_marca "Philips"

# —— Descontos de Mão de Obra —— 

# Lista todos os descontos de mão de obra configurados
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp listar_desconto_mao_obra

# Adiciona desconto de 10% (0.10) para tempo lt(2)
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp adicionar_desconto_mao_obra "lt(2),0.10"

# Edita desconto para tempo gt(4) para 20% (0.20)
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp editar_desconto_mao_obra "gt(4),0.20"

# Remove desconto de mão de obra condicional lt(0.25)
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp remover_desconto_mao_obra "lt(0.25)"


Questão 4 (gestão de clientes):
# Lista todos os clientes guardados em database.pl
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp listar_clientes_bd

# Adiciona cliente ID=8, nome=João, distrito=Coimbra
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp adicionar_cliente 8,João,Coimbra

# Remove o cliente com ID=8
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp remover_cliente 8

# Edita o nome do cliente ID=5 para “Eva+Silva”
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp editar_cliente 5,campo\(nome\),Eva+Silva

# Edita o distrito do cliente ID=5 para “Braga”
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp editar_cliente 5,campo\(distrito\),Braga

# Pesquisa e exibe clientes que residem no distrito Braga
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp pesquisar_cliente_distrito Braga

# Pesquisa e exibe clientes com algum orçamento acima de 100.00 no distrito Lisboa
java -cp "bin:/usr/lib/swi-prolog/lib/jpl.jar" WorkshopApp pesquisar_cliente_orc_superior 100.00,Lisboa