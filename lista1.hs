--todosIguais
todosIguais :: Int -> Int -> Int -> Bool
todosIguais a b c = (a==b)&&(b==c)

--Primeira
osQuatroSaoIguais:: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais a b c d = (a==b)&&(a==c)&&(a==d)

--Segunda
--quantosSaoIguais :: Int -> Int -> Int -> Int
--quantosSaoIguais a b c
-- |(a==b)&&(a==c) = 3
-- |(((a==b)&&(b/=c)) || ((b==c)&&(a/=c)) || ((a==c)&&(a/=b))) = 2
-- | otherwise = 0

--Terceira
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes x y z = (x/=y) && (y/=z) && (x/=z)

--Quarta
--todosDiferentes :: Int -> Int -> Int -> Bool
--todosDiferentes n m p = ((n/=m)&&(m/=p))
-- não compara n com p, podendo estes serem iguais

--Quinta
quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais a b c
 |(todosIguais a b c) = 3
 |(todosDiferentes a b c) = 0
 |otherwise = 2

--Sexta
elevadoDois :: Int -> Int
elevadoDois a = a^2

--Sétima
elevadoQuatro :: Int -> Int
elevadoQuatro a = (elevadoDois a) * (elevadoDois a)

--Oitava
vendas :: Int -> Int
vendas 0 = 1
vendas 1 = 2
vendas 2 = 3
vendas 3 = 4
vendas 4 = 5
vendas 5 = 6
vendas 6 = 7
vendas _ = 8

vendaTotal :: Int -> Int
vendaTotal 0 = vendas 0
vendaTotal n = vendas n + vendaTotal (n-1)