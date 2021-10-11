--menu :: Jogadores -> IO Jogadores
menu dados = do
 system "cls"
 putStrLn "\t***\tJogo da Memória\t***\t\n"
 putStrLn "Insira o numero conforme sua escolha\n"
 putStrLn "(1) Para Jogar\n"
 putStrLn "(2) Para Créditos\n"
 putStrLn "(3) Como Funciona?\n"
 putStrLn "(0) Para Sair\n"
 putStr "Escolha: "
 op <- getChar
 getChar
 operacaoEscolha dados op

operacaoEscolha :: Jogadores -> Char -> IO Jogadores
operacaoEscolha escolha '1' = jogar escolha
operacaoEscolha escolha '2' = creditos
operacaoEscolha escolha '3' = instrucoes
operacaoEscolha escolha '0' = putStrLn "Obrigado por Jogar!"
operacaoEscolha escolha '_' = do
 putStrLn "Opção Inválida\nDigite Enter para voltar ao menu"
 getChar
 menu escolha

