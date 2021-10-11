--Primeira
--head (a:x) = a
head :: [a] -> a
head (x:xs) = x

--tail (a:x) = x
tail :: [a] -> a
tail (x:xs) = x

--fst (t,u) = t
fst :: (a,b) -> a
fst (a,b) = a

soma:: Int->Int->Int
soma a b = a + b

--shift ((a,b),c) = (a,(b,c))

--Segunda
concatena :: [a] -> [a]
concatena [] = [] 
concatena (x:xs) = x : concatena xs

--Terceira
inverte :: [a] -> [a]
inverte [] = [] 
inverte (x:xs) = inverte xs ++ [x]

--Quarta
zipp3 :: [a] -> [a] -> [a] -> [(a,a,a)]
zipp3 [] x y = []
zipp3 x [] y = []
zipp3 x y [] = []
zipp3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zipp3 xs ys zs

--Quinta
mapMaisUm :: (a->b) -> [a] -> [b]
mapMaisUm f [] = []
mapMaisUm f (x:xs) = f (x) : mapMaisUm f xs
--mapMaisUm f (x:xs) = f (x+1) : mapMaisUm f xs


--Sexta 
--foldr :: (Int->Int->Int) -> Int -> [Int] -> Int
--foldr :: (numero->numero->resultado) -> numero -> lista -> resultado
--foldr :: (a->b->b) -> b -> [a] -> b	
--porque foldr funciona tanto para numero inteiros quanto flutuantes 
--podendo ser a ou b do tipo inteiro ou flutuante 