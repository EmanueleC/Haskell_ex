import System.IO

getCh = do
          hSetEcho stdin False
          x <- getChar
          hSetEcho stdin True
          return x

sgetLine = do
            x <- getCh
            if x == '\n' then
              do
                putChar x
                return []
            else
              do
                putChar '-'
                xs <- sgetLine
                return (x:xs)

hangman = do
            putStrLn "Think a word: "
            word <- sgetLine
            putStrLn "Try to guess it: "
            play word

play word = do
              putChar '?'
              guess <- getLine
              let result = fun1 word guess
              putStrLn result
              if (guess == word) then putStr "You got it!"
              else
                play word

fun1 word guess = map fun4 (map fun2 lst)
                  where
                    lst = [[(x,y) | x <- guess] | y <- word]

fun2 lst = map fun3 t
           where
             t = [ t | t <- lst]

fun3 t = if fst (t) == snd (t) then fst(t) else '-'

fun4 "" = '-'
fun4 (x:xs) = if x == '-' then fun4 xs else x
