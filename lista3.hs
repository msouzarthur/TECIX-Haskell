--zero
zero :: Int -> Bool
zero a
 |(a==0) = True
 |otherwise = False

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

--Maior
maior :: Int -> Int -> Int -> Int
maior a b c 
 |(a>b)&&(a>c) = a
 |(b>a)&&(b>c) = b
 |(c>a)&&(c>b) = c
 |otherwise = -1

--Menor
menor:: Int -> Int -> Int -> Int
menor a b c
 |(a<b)&&(a<c) = a
 |(b<a)&&(b<c) = b
 |(c<a)&&(c<b) = c
 |otherwise = -1

--Primeira
somaTuplas :: ((Int, Int), (Int, Int)) -> Int
somaTuplas ((a, b), (c, d)) = ((a+b) + (c+d))

--Segunda
shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift ((a, b), c) = (a, (b, c))

--Terceira
minEmax :: Int -> Int -> Int -> (Int, Int)
minEmax a b c = ((menor (a) (b) (c)), (maior (a) (b) (c)))

--Quarta
zeroVenda :: Int -> (Int, Bool)
zeroVenda a 
 |(zero(vendas a)) = (a, True)
 |(vendas a /=0) = (-1, False)

--Quinta
type Livro = (String, String, Int)

livro :: Livro
livro = ("The Shining", "Stephen King", 9783161484100)

titulo :: Livro -> String
titulo (t,a,n) = t

autor :: Livro -> String
autor (t,a,n) = a

numero :: Livro -> Int
numero (t,a,n) = n 