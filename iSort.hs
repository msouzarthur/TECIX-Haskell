iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins a [] = [a]
ins a (x:xs)
 |a<=x = a:x:xs
 |otherwise = x:ins a xs

{-
maiorEmenor :: [Int] -> (Int,Int)
maiorEmenor [] = [] 
maiorEmenor (x:xs) = (Maior x (xs), Menor x (xs))

Maior :: Int -> [Int] -> Int
Maior a (x:xs) 
 |(a>x) = Maior a xs
 |otherwise = a

Menor :: Int -> [Int] -> Int
Menor a (x:xs)
 |(a<x) = Menor a xs
 |otherwise = a

--insDecrescente :: Int -> [Int] -> [Int]
-}