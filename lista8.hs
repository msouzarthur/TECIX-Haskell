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

--Incrementa
incrementa :: Int -> Int
incrementa x = x+1

--Dobra
dobra :: Int -> Int
dobra x = x*x

--Soma
soma :: Int -> Int -> Int
soma x y = x+y

--Mult
mult :: Int -> Int -> Int
mult x y = x*y

--Primeira
aplicaDuasVezes :: (Int -> Int) -> Int -> Int
aplicaDuasVezes f 0 = 0
aplicaDuasVezes f x = f (f x) 

--Segunda
vendaTotal :: (Int -> Int) -> Int -> Int
vendaTotal f 0 = vendas 0
vendaTotal f x = vendas x + vendaTotal f (x-1)
vendaTotal f x
 |(x<0) = 0

--Terceira
foldInt :: (Int -> Int -> Int) -> [Int] -> Int
foldInt f [] = error "erro"
foldInt f (x:[]) = x
foldInt f (x:xs) = f x (foldInt f xs)

--mapInt
mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f [] = []
mapInt f (x:xs) = f x : mapInt f xs

--Quarta
naoEspaco :: Char -> Bool -- true se for letra
naoEspaco x = x /= ' ' 

filterString :: (Char -> Bool) -> [Char] -> [Char]
filterString f [] = []
filterString f (x:xs) 
 |f x = x : filterString f xs
 |otherwise = filterString f xs

--Quinta 
somaQuadrado :: [Int] -> Int
somaQuadrado [] = error "vazio"
somaQuadrado (x:xs) = foldInt soma (mapInt dobra (x:xs))

--Sexta
iter :: (Int -> Int) -> Int -> [Int]
iter f x = x:iter f(f x)