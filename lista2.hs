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
maxVenda :: Int -> Int
maxVenda a = achaSemana (maiorVenda a) (a)

--Quarta
zeroVendas :: Int -> Int
zeroVendas a 
 |(a<0) = -1
 |(vendas a == 0) = a
 |(vendas a /= 0) = zeroVendas (a-1)

--Quinta
achaSemana :: Int -> Int -> Int
achaSemana a b
 |(b<0) = -1
 |(vendas b == a) = b
 |(vendas b /= a) = achaSemana (a) (b-1)

--Sexta
--chamando a funçao como: achaSemana 0 n
--onde n seria o numero referente a ultima semana cadastrada
--visto que achaSemana pode cumprir o mesmo papel de zeroVendas
--zero :: Int -> Int
--zero a = achaSemana 0 a

---------------------------------------------------------------
{-Sétima
maiorVenda :: Int -> Int -> Int
maiorVenda m n = maxi(vendas m) (vendas n)

maxVenda :: Int -> Int -> Int
maxVenda m n = achaSemana

zeroVendas :: Int -> Int -> Int
zeroVendas m n 
 |(m>n) = -1
 |(vendas n == 0) = n
 |(vendas n /= 0) = zeroVendas (m) (n-1)

achaSemana :: Int -> Int -> Int -> Int
achaSemana m n s
 |(m>n) = -1
 |(vendas n == s) = n
 |(vendas n /= s) = achaSemana (m) (n-1) (s)
-}
---------------------------------------------------------------

--Oitava
fatorial :: Int -> Int
fatorial 0 = 1
fatorial a = a * fatorial (a-1)

--Nona
fatorial2 :: Int -> Int -> Int
fatorial2 a b 
 |(a > b) = a * fatorial2 (a-1) (b)
 |otherwise = a

calculo :: Int -> Int -> Int
calculo a b 
 |(a==b) = a * b
 |(a>b) = fatorial2 (a) (b)
 |(b>a) = fatorial2 (b) (a)

--Décima
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib a = fib (a-1) + fib (a-2)