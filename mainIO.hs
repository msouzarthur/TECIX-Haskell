main :: IO ()
main = do
    putStrLn "Qual o seu nome?"
    nome <- getLine
    putStrLn ("Oi " ++ nome ++ "!!")

leNomeESobrenome :: IO String
leNomeESobrenome = do
     putStrLn  "Qual o Seu nome?"
     nome <- getLine
     putStrLn  "Qual o Seu sobrenome?"
     sob <- getLine
     return (nome ++ " " ++ sob)

le :: IO ()
le = do
   resp <- leNomeESobrenome
   putStrLn resp


ordenaNomes :: IO ()
ordenaNomes = do
  nomes <- leNomes
  putStrLn (show (iSort nomes))


leNomes :: IO [String]
leNomes = do
   nome <- getLine
   if (nome == "")
     then return []
     else do
        nomes <- leNomes
        return (nome : nomes)

--iSort :: Ord a =>  [a] -> [a]
iSort []    = []
iSort (a:x) = ins a (iSort x)

ins :: Ord a => a -> [a] -> [a]
ins a []   = [a]
ins a (b:x)
   | a <= b    = a:(b:x)
   | otherwise = b: ins a x
