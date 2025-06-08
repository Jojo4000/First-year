% Item em inventário
% ID, Nome, Marca, Tipo, Custo, Preço Venda, Quantidade
item(1, 'Oleo Helix 10W40', 'Shell', 'Oleos', 18.50, 25.99, 15).
item(2, 'Conjunto Velas N60', 'Bosh', 'Velas', 5.00, 7.99, 10).
item(3, 'Bateria A7 Silver Dynamic', 'Varta', 'Baterias', 150.00, 199.00, 5).
item(4, 'Anticongelante 5L 10 Rosa', 'IADA', 'Anticongelantes', 3.00, 4.49, 15).
item(5, 'Anticongelante 5L Energy Plus', 'Krafft', 'Anticongelantes', 12.00, 16.99, 15).
item(6, 'Lâmpada H4 60/55W', 'Philips', 'Lâmpadas', 6.50, 9.49, 18).
item(7, 'Correia Dentada', 'Gates', 'Correias', 25.00, 34.99, 10).
item(8, 'Oleo Edge 5W30', 'Castrol', 'Oleos', 35.00, 45.99, 15).
item(9, 'Kit de Embreagem Performance', 'Sachs', 'Kits Embreagem', 210.00, 269.99, 4).
item(10, 'Efficientgrip 205 55 R16', 'Goodyear', 'Pneus', 55.00, 72.59, 15).
item(11, 'Ultracompact 205 55 R17', 'Continental', 'Pneus', 60.00, 79.59, 8).
item(12, 'Anticongelante 5L 10 Verde', 'IADA', 'Anticongelantes', 3.00, 4.49, 9).
item(13, 'Conjunto Velas N35', 'Bosh', 'Velas', 3.50, 4.99, 20).
item(14, 'Lâmpada H7 55W', 'Osram', 'Lâmpadas', 5.00, 7.99, 20).
item(15, 'Filtro de Ar', 'MANN-FILTER', 'Filtros Ar', 10.00, 14.99, 18).
item(16, 'Pastilhas de Freio Dianteiras', 'TRW', 'Pastilhas Travoes', 25.00, 34.99, 12).
item(17, 'Discos de Freio Ventilados', 'Brembo', 'Discos Travoes', 60.00, 79.99, 8).
item(18, 'Correia Poly-V', 'Contitech', 'Correias', 18.50, 24.99, 12).
item(19, 'Kit de Embreagem', 'LUK', 'Kits Embreagem', 190.00, 249.99, 5).
item(20, 'Cabo de Acelerador', 'Valeo', 'Cabos Aceleracao', 14.00, 19.99, 7).
item(21, 'Filtro de Oleo', 'MANN-FILTER', 'Filtros Oleo', 7.00, 9.99, 25).
item(22, 'Pneu 195 65 R15', 'Michelin', 'Pneus', 65.00, 84.99, 10).
item(23, 'Cabo de Bateria 25mm', 'Bosch', 'Cabos Bateria', 22.00, 29.99, 8).
item(24, 'Correia Alternador', 'Dayco', 'Correias', 20.00, 27.99, 15).
item(25, 'Lâmpada LED T10', 'Bosch', 'Lâmpadas', 8.00, 11.99, 12).
item(26, 'Cabo de Embreagem', 'LUK', 'Cabos Aceleracao', 18.00, 24.99, 6).
item(27, 'Pastilhas de Freio Traseiras', 'Ferodo', 'Pastilhas Travoes', 20.00, 29.99, 10).
item(28, 'Discos de Freio Sólidos', 'Bosch', 'Discos Travoes', 55.00, 74.99, 7).
item(29, 'Filtro de Cabine', 'Bosch', 'Filtros Ar', 15.00, 20.99, 10).
item(30, 'Pneu 215 60 R16', 'Pirelli', 'Pneus', 70.00, 94.99, 7).
item(31, 'Bateria AGM Start-Stop', 'Bosch', 'Baterias', 180.00, 239.99, 6).
item(32, 'Conjunto Velas Iridium', 'NGK', 'Velas', 12.00, 15.99, 8).
item(33, 'Oleo Quartz 9000 5W30', 'Total', 'Oleos', 32.00, 42.99, 10).


% Serviços mecânicos
% ID, Nome, Categorias Utilizadas, Número de Mecânicos, Tempo Necessário (horas), Custo Base Serviço
servico(1, 'Troca de Oleo', ['Oleos', 'Filtros Oleo'], 1, 0.5, 0.00).
servico(2, 'Substituicao de Velas', ['Velas'], 1, 1.0, 0.00).
servico(3, 'Troca de Bateria', ['Baterias'], 1, 0.5, 0.00).
servico(4, 'Limpeza Sistema de Arrefecimento', ['Anticongelantes'], 1, 1.0, 0.00).
servico(5, 'Troca de Lampada', ['Lâmpadas'], 1, 0.25, 0.00).
servico(6, 'Substituicao de Correia', ['Correias'], 2, 3.0, 0.00).
servico(7, 'Troca de Pneu', ['Pneus'], 2, 1.5, 0.00).
servico(8, 'Substituicao de Filtro de Ar', ['Filtros Ar'], 1, 0.5, 0.00).
servico(9, 'Substituicao de Pastilhas de Travoes', ['Pastilhas Travoes'], 2, 2.0, 0.00).
servico(10, 'Troca de Discos de Travoes', ['Discos Travoes'], 2, 2.5, 0.00).
servico(11, 'Troca de Kit de Embreagem', ['Kits Embreagem'], 2, 4.0, 0.00).
servico(12, 'Troca de Cabos de Aceleracao', ['Cabos Aceleracao'], 1, 1.5, 0.00).
servico(13, 'Troca de Cabos de Bateria', ['Cabos Bateria'], 1, 1.0, 0.00).
servico(14, 'Serviço de Reboque', [], 1, 1.0, 120.00).
servico(15, 'Lavagem de Motor', [], 1, 1.0, 50.00).
servico(16, 'Revisao Geral', ['Oleos', 'Filtros Oleo', 'Velas', 'Pastilhas Travoes', 'Filtros Ar', 'Anticongelantes'], 2, 4.0, 0.00).

% Descontos por marca de item
% marca, desconto
desconto_marca('Michelin', 0.1).
desconto_marca('Bosch', 0.15).
desconto_marca('Philips', 0.3).

% Descontos mao de obra
% tempo (horas), desconto
desconto_mao_obra(lt(0.25), 0.05).
desconto_mao_obra(gt(4),    0.15).

% mecânicos
% ID, Nome, Custo Hora
mecanico(1, "Ganacio", 10.00).
mecanico(2, "Severo", 8.00).

% — Clientes do sistema — 
% cliente(ID, Nome, Distrito).
cliente(1, 'Alice', 'Aveiro').
cliente(2, 'Beatriz', 'Braga').
cliente(3, 'Carlos', 'Coimbra').
cliente(4, 'Diogo', 'Lisboa').
cliente(5, 'Eva', 'Porto').
cliente(6, 'Francisca', 'Faro').
cliente(7, 'Guilhermina', 'Viseu').

% — Orçamentos realizados — 
% orcamento(ID, ClienteID, data(Dia,Mes,Ano), Servicos, Itens, ValorTotal, Pago).
orcamento(1, 1, data(20,5,2025), [1,2],   [1,21,2,13],  85.95, true).
orcamento(2, 2, data(21,5,2025), [4,1],   [4,12,1,21],  62.96, true).
orcamento(3, 3, data(22,5,2025), [14],    [],           120.00, true).
orcamento(4, 4, data(23,5,2025), [15],    [],            50.00, false).
orcamento(5, 5, data(23,5,2025), [5,16],  [6,14,25,1,21,2,13,16,27,4,12], 543.80, true).
orcamento(6, 6, data(25,5,2025), [1],     [8,21],        60.97, false).
orcamento(7, 7, data(26,5,2025), [2],     [32],          23.99, true).
orcamento(8, 3, data(27,5,2025), [7],     [10,11],      168.17, false).
orcamento(9, 4, data(28,5,2025), [16],    [1,21,2,13,16,27,4,12,15,29], 416.85, false).
orcamento(10,1, data(28,5,2025), [1,8,3,5], [1,21,15,3,6], 322.44, true).