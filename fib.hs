--Vendas
vendas :: Int -> Int
vendas 0 = 4
vendas 1 = 2
vendas 2 = 3
vendas 3 = 4
vendas 4 = 5
vendas 5 = 6
vendas 6 = 7
vendas _ = 8

--Primeira
maxi :: Int -> Int -> Int
maxi a b 
 |(a>=b) = a
 |(b>a) = b

--Segunda
maiorVenda :: Int -> Int
maiorVenda 0 = vendas 0
maiorVenda a = maxi (vendas a) (vendas 0)

--Terceira
--maxVenda :: Int -> Int

--Quarta
--zeroVendas :: Int -> Int

--Quinta
--achaSemana :: Int -> Int -> Int 
