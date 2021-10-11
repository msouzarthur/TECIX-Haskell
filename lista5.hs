--iSort
iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)
ins :: Int -> [Int] -> [Int]
ins a [] = [a]
ins a (x:xs)
 |a<=x = a:x:xs
 |otherwise = x:ins a xs

--Primeira
membro :: Int -> [Int] -> Bool
membro a [] = False
membro a (x:xs)
 |(a==x) = True
 |otherwise = membro a xs

--Segunda
membroNum :: Int -> [Int] -> Int --Recebe um numero e uma lista
membroNum a [] = 0 --responde o numero de vezes que aparece
membroNum a (x:xs) 
 |(a==x) = 1 + (membroNum a xs) 
 |otherwise = membroNum a xs

{- --Terceira
membro :: Int -> [Int] -> Bool
membro a [] = False
membro a (x:xs)
 |(membroNum a (x:xs))>0 = True
 |otherwise = False-}

--Quarta
unico :: [Int] -> [Int]
unico [] = []
unico (x:xs) 
 |(membroNum x (iSort(x:xs)))==1 = x:unico(xs)
 |(membroNum x (iSort(x:xs)))>1 = remover x (xs)

remover :: Int -> [Int] -> [Int]
remover a (x:xs) 
 |(a==x) = remover a xs
 
--nova :: Int -> [Int]
--nova a : [a]
--Quinta
