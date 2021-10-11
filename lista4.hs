--Primeira
multDoisLista :: [Int] -> [Int]
multDoisLista [] = []
multDoisLista (x:xs) = (x*2) : multDoisLista xs

--Segunda
tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (x:xs) = (1) + tamanho xs

--Terceira
produtoLista :: [Int] -> Int
produtoLista [] = 1
produtoLista (x:xs) = x * produtoLista xs

--Quarta
andLista :: [Bool] -> Bool
andLista [] = True
andLista (x:xs) = x && andLista xs

--Quinta
concatLista :: [[Int]] -> [Int]
concatLista [] = []
concatLista (x:xs) = x ++ concatLista xs

--Sexta
inverteLista :: [Int] -> [Int]
inverteLista []=[]
inverteLista (x:xs) = (inverteLista xs) ++ [x]
