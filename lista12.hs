data Arvore a = Folha a | Nodo a (Arvore a) (Arvore a)
 deriving (Eq,Show)

arv :: Arvore Int
arv = Nodo 1 (Nodo 2 (Nodo 3 (Folha 1) (Folha 2)) (Folha 3)) (Folha 4)

--Primeira
multDois :: Arvore Int -> Arvore Int
multDois (Folha a) = (Folha (2*a))
multDois (Nodo a n1 n2) = (Nodo (2*a) (multDois n1) (multDois n2))

--Segunda
contaElementos :: Arvore a -> Int
contaElementos (Folha a) = 1
contaElementos (Nodo a n1 n2) = 1 + contaElementos n1 + contaElementos n2

--Terceira
altura :: Arvore a -> Int
altura (Folha a) = 0 
altura (Nodo a n1 n2) = 1 + altura n1 + altura n2

{-
--Quarta
maiorElemento :: Arvore Int -> Int
maiorElemento arvore = quickSort(arvoreToLista arvore)

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = quickSort (menores x xs) ++ [x] ++ quickSort(maiores x xs)

menores :: Int -> [Int] -> [Int]
menores n [] = []
menores n (x:xs) = if(x<=n) then x : menores n xs else menores n xs

maiores :: Int -> [Int] -> [Int]
maiores n [] = []
maiores n (x:xs) = if(x>n) then x : maiores n xs else menores n xs
-}

--Quinta
procuraInt :: Int -> Arvore Int -> Bool
procuraInt numero (Folha a) = if (numero == a) then True else False
procuraInt numero (Nodo a n1 n2) = if (numero == a) then True else False || (procuraInt numero n1) || (procuraInt numero n2)

--Sexta
quantasVezes :: Int -> Arvore Int -> Int
quantasVezes numero (Folha a) = if(numero==a) then 1 else 0
quantasVezes numero (Nodo a n1 n2) = if(numero==a) then 1+quantasVezes numero n1 + quantasVezes numero n2 else quantasVezes numero n1 + quantasVezes numero n2 

--Sétima
refleteArvore :: Arvore a -> Arvore a
refleteArvore (Folha a) = Folha a
refleteArvore (Nodo a es dr) = (Nodo a (refleteArvore dr) (refleteArvore es))

--Oitava
arvoreToLista :: Arvore a -> [a]
arvoreToLista (Folha a) = [a]
arvoreToLista (Nodo a n1 n2) = arvoreToLista(n1) ++ [a] ++ arvoreToLista(n2)

--Nona
mapTree :: (a->b) -> Arvore a -> Arvore b
mapTree f (Folha a) = Folha(f a)
mapTree f (Nodo a n1 n2) = Nodo(f a) (mapTree f n1) (mapTree f n2)