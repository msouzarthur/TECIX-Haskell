module Menu where

import Data.Char
import Data.List
import System.IO
import System.Random
import Control.Monad.State

data Jogo = Jogo {
      gsResposta  :: String,        -- resposta
      gsConhece   :: [Maybe Char],  -- resposta parcial do conhecimento do jogador
      gsLetraInv :: [Char],         -- letras invalidas adivinhadas até ao momento
      gsAdivinha   :: Int,          -- numero de adivinhas
      gsGanhaPerde :: Maybe Bool    -- ganhou = true, perdeu = false
    }
  deriving (Show)

novoJogo :: String -> Jogo
novoJogo resposta = Jogo{
                   gsResposta = map toUpper resposta,
                   gsConhece = (map (filter $ not . isAlpha) resposta),
                   gsLetraInv = [],
                   gsAdivinha = 0,
                   gsGanhaPerde = Nothing}

data JogadorInsere= JIAdivinha Char | JISair | JINovoJogo | JIActualizar
  deriving (Show)

menu :: IO ()
menu = do
  hSetBuffering stdout NoBuffering
  putStr "Bem-vindo ao jogo da forca!\n\n"
  putStr instruccoes
  runStateT inicioNovoJogo undefined
  return ()

inicioNovoJogo :: StateT Jogo IO ()
inicioNovoJogo = do
  nPalavra <- liftIO $ getStdRandom (randomR (0,length palavraListaa - 1))
  let palavra = palavraListaa !! nPalavra
  let gs = novoJogo palavra
  put gs
  liftIO $ putStrLn $ daJogo gs
  loopJogo

loopJogo :: StateT Jogo IO ()
loopJogo = do
  ji <- liftIO getJogadorInsere
  case ji of
    JIAdivinha c -> do
      modify $ handleAdivinha c
      gs <- get
      liftIO $ putStrLn $ daJogo gs
      case (gsGanhaPerde gs) of
        Nothing -> loopJogo
        Just True -> do
          liftIO $ putStrLn "Parabens, ganhou!"
          inicioNovoJogo
        Just False -> do
          liftIO $ putStrLn "Foi enforcado!"
          liftIO $ putStrLn $ "A palavra era \'" ++ (gsResposta gs) ++"\'."
          inicioNovoJogo
    JISair -> do
      gs <- get
      liftIO $ putStrLn $ "A palavra era \'" ++ (gsResposta gs) ++ "\'."
      liftIO $ putStrLn "Obrigado por jogar a forca!"
    JINovoJogo -> do
      gs <- get
      liftIO $ putStrLn $ "A palavra era \'" ++ (gsResposta gs) ++ "\'."
      inicioNovoJogo
    JIActualizar -> do
      gs <- get
      liftIO $ putStrLn $ daJogo gs
      loopJogo

getJogadorInsere :: IO JogadorInsere
getJogadorInsere = do
  putStr "Forca> "
  reaccao <- getLine
  if null reaccao
    then getJogadorInsere
    else do
      let c:cs = reaccao
      if isAlpha c
        then return $ JIAdivinha $ toUpper c
        else if c == ':' && not (null cs)
          then case toLower (head cs) of
                 'q' -> return JISair
                 'n' -> return JINovoJogo
                 'r' -> return JIActualizar
                 '?' -> do
                   putStr instruccoes
                   getJogadorInsere
                 otherwise -> do
                   putStrLn $ "Comando desconhecido \'" ++ cs ++ "\'"
                   putStrLn $ "Usar \':?\' para ajuda."
                   getJogadorInsere
          else do
            putStrLn $ "Entrada invalida \'" ++ reaccao ++ "\'"
            putStrLn $ "Usar \':?\' para ajuda."
            getJogadorInsere

instruccoes :: String
instruccoes =
    "Instrucoes:\n"
++ "Para adivinhar a letra, inserir a mesma e carregar em ENTER.\n"
++ "Para sair ou reiniciar jogo, usar os segjintes comandos:\n"
++ "  :q = sair\n"
++ "  :n = novo jogo\n"
++ "  :r = reiniciar\n"
++ "  :? = mostrar instruccoes\n"
++ "\n"

--filter :: (a -> Bool) -> a -> Maybe a
--filter pred x = if pred x then Just x else Nothing

handleAdivinha :: Char -> Jogo -> Jogo
handleAdivinha ch state =
    if (elem ch $ gsLetraInv state)
      then state
      else
    if (elem ch $ gsResposta state)
      then let revela = map (filter (== ch)) (gsResposta state)
               sabe = zipWith mplus (gsConhece state) revela
               ganhou = all (maybe False (const True)) sabe
           in state{gsConhece = sabe, gsGanhaPerde = filter id ganhou}
      else let incorrecto = 1 + (gsAdivinha state)
           in state{gsLetraInv = ch:(gsLetraInv state),
                    gsAdivinha = incorrecto,
                    gsGanhaPerde = filter not (incorrecto < 7)}

palavraListaa :: [string]
palavraListaa = ["crocodilo", "leaomarinho", "formiga", "urso", "bufalo",
            "borboleta", "canario", "camaleao", "mosca", "dinossauro",
            "cao", "golfinho", "foca", "elefante", "flamingo", "sapo",
            "girafa", "tubarao", "hipopotamo", "cavalo", "iguana",
            "canguro", "camelo", "lama", "rato", "panda", "pelicano",
            "koala", "tigre", "leao", "rinoceronte", "salamandra",
            "cavalo marinho", "cobra", "tucano", "cabra", "zebra",
		"Berlim", "Andorra", "Azerbaijao", "Bruxelas", "Sarajevo",
		"Chipre", "Croacia", "Zagreb", "Dinamarca", "Copenhaga", "Bratislava",
		"Eslovenia", "Espanha", "Holanda", "Hungria", "Budapeste", "Irlanda",
		"Dublin", "Islandia", "Italia", "Liechtenstein", "Luxemburgo", "Malta",
		"Moldavia",	"Montenegro", "Noruega", "Oslo", "Londres", "Romenia",
		"Bucareste", "Russia", "Belgrado", "Estocolmo", "Ucrania", "Kiev", "Vaticano"]

daJogo :: Jogo -> String
daJogo gs =
    let noose = renderNoose $ gsAdivinha gs
        report = ["","A palavra:","",palavra,"","Letras escolhidas invalidas:","",guessed]
        palavra = intersperse ' ' $ map (maybe '_' id) (gsConhece gs)
        guessed = gsLetraInv gs
    in (concat $ zipWith (++) noose $ map (++ "\n") report)

renderNoose :: Int -> [string]
renderNoose n | n <= 0 = [
"   ___     ",
"  /   |    ",
"  |        ",
"  |        ",
"  |        ",
"  |        ",
"  |        ",
" -+-       "]
renderNoose 1 = [
"   ___     ",
"  /   |    ",
"  |   O    ",
"  |        ",
"  |        ",
"  |        ",
"  |        ",
" -+-       "]
renderNoose 2 = [
"   ___     ",