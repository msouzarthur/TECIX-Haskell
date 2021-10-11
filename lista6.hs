--iSort
iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)
ins :: Int -> [Int] -> [Int]
ins a [] = [a]
ins a (x:xs)
 |a<=x = a:x:xs
 |otherwise = x:ins a xs

--conta tamanho
conta :: [Int] -> Int
conta [] = 0
conta (x:xs) = 1 + conta xs

--soma
soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = x + soma xs

--divisao
divisao :: Int -> Int -> Int
divisao a x
 |(x<0) = error "impossivel"
 |otherwise = div a x

--achaPosicao
achaPosicao :: Int -> [Int] -> Int
achaPosicao a (x:xs) 
 |(a==x) = (x:xs) !! x
 |(a/=x) = achaPosicao (a+1) xs
 |otherwise = 0

--Primeira
pegaPosicao :: Int -> [Int] -> Int
pegaPosicao a [] = error "Lista vazia"
pegaPosicao a (x:xs) = (x:xs) !! a

--Segunda
pega :: Int -> [Int] -> [Int]
pega a [] = []
pega a (x:xs) 
 |(a>0) = x : pega (a-1) xs
 |otherwise = []

--Terceira
retira :: Int -> [Int] -> [Int]
retira a [] = [] 
retira a (x:xs)
 |(a<length(x:xs)) = ((x:xs) !! a) : retira (a) xs 
 |otherwise = []

--Quarta
mediaLista :: [Int] -> Int
mediaLista [] = 0
mediaLista (x:xs) = divisao (soma (x:xs)) (conta (x:xs))

--Quinta
pegaMaiores :: Int -> [Int] -> [Int]
pegaMaiores a [] = []
pegaMaiores a (x:xs) = pega a (reverse(iSort(x:xs)))

--Sexta
contaMaiores :: Int -> [Int] -> Int
contaMaiores a [] = 0
contaMaiores a (x:xs) 
 |(x>a) = 1+contaMaiores a xs
 |otherwise = contaMaiores a xs

--Sétima
intercala :: [Int] -> [Int] -> [Int]
intercala (a:as) [] = (a:as)
intercala [] (x:xs) = (x:xs)
intercala [] [] = []
intercala (a:as) (x:xs) = a:x:intercala (as) (xs)

--Oitava
dupli :: [Int] -> [Int]
dupli [] = [] 
dupli (x:xs) = x:x:dupli xs

--Nona
repli :: Int -> [Char] -> [Char]
repli 0 (x:xs) = []
repli a [] = [] 
repli a (x:xs) = x: repli (a-1) (x:xs)

--Décima
dropEvery :: Int -> [Char] -> [Char]
dropEvery a (x:xs) = aux (x:xs) a

aux :: [Char] -> Int -> [Char]
aux [] a = []
aux (x:xs) 1 = aux xs 1
aux (x:xs) a = x: aux xs (a-1) 

--Décima Primeira
separa :: Int -> [Char] -> [Char]
separa a [] = []
separa a (x:xs) 
 |(a>0) = x:separa (a-1) xs
 |otherwise = []

split :: Int -> [Char] -> ([Char],[Char])
split a [] = ([],[])
split a (x:xs) 
 |(a>0) = (separa a (x:xs),(xs))
 |otherwise = ([],[])