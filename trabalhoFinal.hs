{-
 jogo -> comanda tudo em ordem
* intro -> pega o numero do usuario
* extrai -> retira esse indice de palavras e volta pra jogo
 play -> recebe a palavra e faz tela de gameplay com forca 0
      -> pede letra pro usuario, se nenhuma letra bater, forca 1
      -> se acertar todos caracter sem chegar a 6, ganhou
      -> se não acertar todos caracter até chegar a 6, perdeu
* ganhou -> tela de Win e fecha
* perdeu -> tela de gameover e fecha
* palavras -> contem as palavras
* forca -> indica o andamento do jogo
* checagem -> indica com True se ele acertou algum caracter
* exibir -> mostra o caracter que foi acertado e dá '_' nos demais
* instrucoes -> instrucoes
-}

-- *JOGO*
jogo :: IO String
jogo = do
 putStrLn(instrucoes)
 numero<-intro
 play (extrai numero palavras) 0
 
-- *PLAY*
play:: String -> Int -> IO [Char]
play palavraEscolhida vida = do
 putStr(forca vida)
 putStrLn(exibir palavraEscolhida (checagem palavraEscolhida ' '))
 putStr("Digite uma letra em minusculo: ")
 letra<-getChar
 if(filtro(checagem palavraEscolhida letra)) then play2 palavraEscolhida vida letra 
 else play2 palavraEscolhida (vida+1) letra 
 

 --then play palavraEscolhida vida ;
 --exibir palavraEscolhida (checagem palavraEscolhida letra)
 --else play palavraEscolhida (vida+1)         --Significa que não tem a letra
 --return(palavraEscolhida)
 
play2:: String -> Int -> Char ->  IO [Char]
play2 palavraEscolhida vida letra = do
 putStr (forca vida)
 putStrLn (exibir palavraEscolhida (checagem palavraEscolhida letra))
 putStr("Digite uma letra em minusculo: ") 
 getLine
 letra<-getChar
 if(filtro(checagem palavraEscolhida letra)) then play2  palavraEscolhida vida letra
 else play2 palavraEscolhida (vida+1) letra
 
 --if(filtro (checagem palavraEscolhida letra)) 
 -- then return(palavraEscolhida) else return(palavraEscolhida)

 --listaBool<-checagem palavraEscolhida letra
 --putStr(exibir palavraEscolhida listaBool)
 --return(palavraEscolhida)

-- *REVELADOS*
-- *FILTRO*
filtro :: [Bool] -> Bool
filtro [] = False
filtro (x:xs) 
 |(x) = True
 |otherwise = filtro xs

-- *EXIBIR*
exibir :: String -> [Bool] -> String
exibir [] [] = []
exibir (x:xs) (y:ys) 
 |(y) = x: exibir xs ys
 |otherwise = '_':exibir xs ys

-- *CHECAGEM*
checagem :: String -> Char -> [Bool]
checagem [] y = []
checagem (x:xs) y 
 |(x==y) = True:checagem xs y
 |otherwise = False:checagem xs y

-- *INTRO*
intro :: IO Int
intro = do
 putStrLn "Digite um numero entre 0 e 40: "
 num <- getLine
 if(read num) >=0 
  then do
   if(read num) <40
    then do 
     return (read num)
   else intro
 else intro

-- *EXTRAI*
extrai :: Int -> [String] -> String
extrai num lista
 |((num>=0)&&(num<40)) = lista !!num
 |otherwise = error "Posicao invalida" 

-- *PALAVRAS*
palavras ::  [String]
palavras = ["amarelo","amiga","amor","ave","aviao","avo",
 "balao","bebe","bolo","branco",
 "cama","caneca","celular","clube","copo",
 "doce","elefante","escola","estojo","faca","foto",
 "garfo","geleia","girafa","janela","limonada","mae","meia","noite",
 "oculos","onibus","ovo","pai","pao","parque","passarinho",
 "peixe","pijama","rato","umbigo"] 

-- *FORCA*
forca :: Int -> String
forca 0 = "  __   \n |  |  \n |     \n |      \n |     \n |     \n |     \n |     \n---    \t\t"
forca 1 = "  __   \n |  |  \n |  O  \n |      \n |     \n |     \n |     \n |     \n---    \t\t"
forca 2 = "  __   \n |  |  \n |  O  \n |  |   \n |     \n |     \n |     \n |     \n---    \t\t"
forca 3 = "  __   \n |  |  \n |  O  \n |  |-- \n |     \n |     \n |     \n |     \n---    \t\t"
forca 4 = "  __   \n |  |  \n |  O  \n |--|-- \n |     \n |     \n |     \n |     \n---    \t\t"
forca 5 = "  __   \n |  |  \n |  O  \n |--|-- \n |  |  \n | !   \n |     \n |     \n---    \t\t"
forca 6 = "  __   \n |  |  \n |  O  \n |--|-- \n |  |  \n | ! ! \n |     \n |     \n---    \t\t"

-- *INSTRUÇÕES*
instrucoes :: String
instrucoes = ("\t\tSeja bem vindo!\nEste eh um jogo da forca e funciona da seguinte maneira:\n1° - Digite jogo e insira um numero para selecionar uma palavra\n2° - Insira letras para ver se elas estao contidas na palavra\n3° - Voce tem no maximo 6 erros\n\t\tBoa Sorte!\n")

-- *GANHOU*
ganhou :: String
ganhou = ("\nMEUS PARABENS, VOCE CONSEGUIU VENCER!\nSINTA-SE A VONTADE PARA JOGAR DE NOVO\n")

-- *PERDEU*
perdeu :: String 
perdeu = ("\nINFELIZMENTE VOCE NAO CONSEGUIU VENCER!\nSINTA-SE A VONTADE PARA JOGAR DE NOVO\n")
