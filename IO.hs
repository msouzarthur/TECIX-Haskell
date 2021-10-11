leNomeEnd :: IO (String,String)
leNomeEnd = do
 putStrLn "digite seu nome: "
 nome <- getLine
 putStrLn "digite se endereco: "
 endereco <- getLine
 return (nome,endereco)

leNNE :: Int -> IO [(String,String)]
leNNE vezes 
 |(vezes>=0) = leNNE (vezes-1) (leNomeEnd)
 
leNumero :: IO Int
leNumero = do
 putStr "digite um numero: "
 numero <- getLine
 return (read numero)

printLista :: [Int] -> IO()
printLista = do
 get