--Vendas
vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 0
vendas 2 = 15
vendas 3 = 50
vendas 4 = 51
vendas 5 = 25
vendas 6 = 7
vendas _ = 8

vendaTotal :: Int -> Int
vendaTotal 0 = vendas 0
vendaTotal n = vendas n + vendaTotal (n-1)

--Chama tudo
tabela :: Int -> String
tabela a = cabecalho ++ geraVendas a ++ geraTotal a

--Header
cabecalho :: String
cabecalho = "Semana\t\tVendas\n"

--modelo genérico de exibição
geraTodos :: Int -> String
geraTodos a = "Semana " ++ show a ++ "\t" ++ show (vendas a) ++ "\n"

--Dá as vendas / geraVendas de x, passando pra geraTodos, e chama o anterior
geraVendas :: Int -> String
geraVendas 0 = geraTodos 0
geraVendas a = geraVendas(a-1) ++ geraTodos a

--Soma tudo
geraTotal :: Int -> String
geraTotal a = "Total:\t\t" ++ show(vendaTotal a) ++ "\n"