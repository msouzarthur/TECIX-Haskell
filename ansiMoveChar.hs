import System.Console.ANSI
import System.IO




main :: IO ()
main =  do hSetEcho stdin False
           hideCursor
           clearScreen
           hPutChar stdout '*'
           move

move :: IO ()
move = do  c <- hGetChar stdin
           case  c of
             'r' -> moveUp
             'c'  -> moveDown
             'd' -> moveLeft
             'f' -> moveRight
             _  -> return ()



moveUp :: IO ()
moveUp = do 
       clearScreen
       hCursorBackward stdout 1
       hCursorUp stdout 1
       hPutChar stdout '*'
       move

moveDown :: IO ()
moveDown = do 
       clearScreen
       hCursorBackward stdout 1
       hCursorDown stdout 1
       hPutChar stdout '*'
       move
moveLeft :: IO ()
moveLeft = do 
       clearScreen
       hCursorBackward stdout 1
       hCursorBackward stdout 1
       hPutChar stdout '*'
       move

moveRight :: IO ()
moveRight = do 
       clearScreen
       hCursorForward stdout 1
       hPutChar stdout '*'
       move