--Quadrado 
quadrado :: Int -> Int
quadrado x = x*x

--Primeira
concatena :: [[a]] -> [a]
concatena [] = []
concatena (x) = foldr (++) [] x

--Segunda
andLista :: [Bool] -> Bool 
andLista [] = error "vazio"
andLista (x) = foldr (&&) True x 

--Terceira
somaQuadPos :: [Int] -> Int
somaQuadPos (x) = foldr (+) 0 ( map (quadrado) (filter (>0) x) ) 

--Quarta
somaListas :: [[Int]] -> Int
somaListas listas = foldr (+) 0 (map (*1) (concatena (listas)))
somaListas2 :: [[Int]] -> Int
somaListas2 listas = foldr (+) 0 (concat (listas))

--Quinta
tamanhoListas :: [[a]] -> Int
tamanhoListas listas = foldr (+) 0 (map length (listas))

--Sexta **
inverte :: [a] -> [a]
inverte [] = [] 
inverte (x:xs) = inverte xs++[x]

--inverte :: [a] -> [a]
--inverte = foldl (flip(:)) [] 
--reverse_foldr :: [a] -> [a]  
--reverse_foldr s = foldr (\x xs -> xs ++ [x]) [] s 

--Sétima 
separaPalavras :: [Char] -> [[Char]]
separaPalavras [] = []
separaPalavras lista = takeWhile (/=' ') lista : separaPalavras(drop 1 (dropWhile (/=' ') lista))