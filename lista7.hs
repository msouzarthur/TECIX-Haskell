--Primeira
somaQuadrupla :: [(Int,Int,Int,Int)] -> Int
somaQuadrupla [] = 0
somaQuadrupla ((a,b,c,d):xs) = a+b+c+d+somaQuadrupla xs

--Segunda
somaTuplas ::  [((Int,Int),(Int,Int))] -> Int
somaTuplas [] = 0
somaTuplas (((a,b),(c,d)):xs) = a+b+c+d+somaTuplas xs

--Terceira
zipp :: [Int] -> [Int] -> [(Int, Int)]
zipp [] (a:as) = []
zipp (x:xs) [] = []
zipp [] [] = []
zipp (x:xs) (a:as) = (x,a) : zipp (xs) (as)

--Quarta 
zipTres :: [Int] -> [Int] -> [Int] -> [(Int,Int, Int)]
zipTres [] _ _ = []
zipTres _ [] _ = []
zipTres _ _ [] = []
zipTres [] [] [] = []
zipTres (x:xs) (a:as) (b:bs) = (x,a,b) : zipTres (xs) (as) (bs)

--Quinta
unZipp :: [(Int,Int)] -> ([Int],[Int])
unZipp [] = ([],[])
unZipp ((a,b):xs) = (unzipEsq((a,b):xs),unzipDir((a,b):xs))

unzipEsq :: [(Int,Int)] -> [Int]
unzipEsq [] = []
unzipEsq ((e,d):xs) = e: unzipEsq xs

unzipDir :: [(Int,Int)] -> [Int]
unzipDir [] = [] 
unzipDir ((e,d):xs) = d: unzipEsq xs