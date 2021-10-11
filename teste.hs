--module Main where
--import Data.Char
--import System.IO
--import System.Random
type GBoard = [[Char]]
type MBoard = [[Bool]]
gBoard :: GBoard
gBoard = [['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-']]
--3 Minas
--(2,2),(4,4),(5,5)
mBoard :: MBoard
mBoard = [[False, False, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, True, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, True , False, False, False, False],
          [False, False, False, False, False, True , False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False]]

gArr :: Int -> [t] -> t
gArr a [] = error "lista vazia"
gArr a (x:xs) = (x:xs) !! a

uArr :: Int -> a -> [a] -> [a]
uArr p v [] = [v]
uArr p v (x:xs)
 |(p>length (x:xs)) = error "posicao inexistente"
 |otherwise = take (p) (x:xs) ++ [v] ++ drop (p+1) (x:xs)
-- |otherwise = take (p-1) (x:xs) ++ [v] ++ drop (p) (x:xs)

gPos :: Int -> Int -> [[a]] -> a
gPos l c (tabuleiro) = gArr c (gArr l tabuleiro) 

uPos :: Int -> Int ->  a -> [[a]] -> [[a]]
uPos l c v (tabuleiro) = take l tabuleiro ++ [uArr c v (gArr l tabuleiro)] ++ drop (l + 1) tabuleiro

isMine :: Int -> Int -> MBoard -> Bool
isMine l c tabuleiro 
 |(gPos l c tabuleiro) = True
 |otherwise = False

isValidPos :: Int -> Int -> Int -> Bool
isValidPos t l c
 |((l>=0)&&(l<t))&&((c>=0)&&(c<t)) = True
 |otherwise = False

geraLista :: Int -> a -> [a]
geraLista n v 
 |(n>0) = v:(geraLista (n-1) v) 
 |otherwise = []

geraNovoTabuleiro :: Int -> GBoard
geraNovoTabuleiro n = geraLista n (geraLista n '-')

geraMapaDeMinasZerado :: Int -> MBoard
geraMapaDeMinasZerado n = geraLista n (geraLista n False)

teste :: [Bool] -> Int
teste [] = 0
teste (x:xs)
 |(x) = 1 + teste xs
 |otherwise = teste xs
contaMinas :: MBoard -> Int
contaMinas mMinas = teste (concat mMinas)

teste2 :: [Char] -> Int
teste2 [] = 0
teste2 (x:xs)
 |(x=='-') = 1+teste2 xs
 |otherwise = teste2 xs
contaFechadas :: GBoard -> Int
contaFechadas mJogo = teste2 (concat mJogo)

endGame :: MBoard -> GBoard -> Bool
endGame minas jogadas
 |((contaMinas minas)==(contaFechadas jogadas)) = True
 |otherwise = False

filtro :: Int -> [(Int,Int)] -> [(Int,Int)]
filtro tamanho [] = []
filtro tamanho ((a,b):xs)   
 |(isValidPos tamanho a b) = (a,b) : filtro tamanho xs
 |otherwise = filtro tamanho xs
aux :: Int -> Int -> Int -> [(Int,Int)]
aux t l c = [(l-1,c-1),(l-1,c),(l-1,c+1),(l,c-1),(l,c+1),(l+1,c-1),(l+1,c),(l+1,c+1)]
validMoves :: Int -> Int -> Int -> [(Int,Int)]
validMoves t l c 
 |((l>=t)||(c>=t)) = error "posicao invalida"
 |otherwise = filtro t (aux t l c)
 
aux2 :: [(Int,Int)] -> MBoard -> Int
aux2 [] mapa = 0
aux2 ((a,b):xs) mapa 
 |(isMine a b mapa) = 1 + aux2 xs mapa
 |otherwise = aux2 xs mapa
cMinas :: Int -> Int -> MBoard -> Int
cMinas l c mapa = aux2(validMoves (length mapa) l c) mapa

printBoard :: GBoard -> String
printBoard mapa = header (0) (length mapa) ++ jogo mapa
-- barraLateral (length mapa)
--mostra o mapa gBoard
jogo :: GBoard -> String
jogo mapa = show mapa

header :: Int -> Int -> [Char]
header zero tamanho 
 |(zero<tamanho) = show (zero) ++ " " ++ header (zero+1) (tamanho)
 |otherwise = []
{-
aux3 :: MBoard -> GBoard -> GBoard
aux3 (m:ms) (j:js) 
 
abreTabuleiro :: MBoard -> GBoard -> GBoard
abreTabuleiro mMinas mJogo 
 |(contaFechadas mJogo>0) = 
 |otherwise

abreJogada :: Int -> Int -> MBoard -> GBoard -> GBoard
abreJogada l c mMina mJogo
 |(gPos l c mMina == True) = endGame mMina mJogo
 |(gPos l c mJogo == ' ') = mJogo
 |(cMinas l c mMina>0) = uPos l c (cMinas l c mMina) mJogo
 |(cMinas l c mMina == 0) = uPos l c 0 mJogo : abreJogada l c mMina mJogo
-}
--printBoard :: GBoard -> String
--abreTabuleiro :: MBoard -> GBoard -> GBoard
--abreJogada :: Int -> Int -> MBoard -> GBoard -> GBoard