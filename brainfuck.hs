import System.Environment (getArgs)
import System.IO (readFile)

type Tape = ([Int], Int, [Int])

initialTape :: Tape
initialTape = (repeat 0, 0, repeat 0)

moveRight :: Tape -> Tape
moveRight (ls, x, r:rs) = (x:ls, r, rs)

moveLeft :: Tape -> Tape
moveLeft (l:ls, x, rs) = (ls, l, x:rs)

inc :: Tape -> Tape
inc (ls, x, rs) = (ls, x + 1, rs)

dec :: Tape -> Tape
dec (ls, x, rs) = (ls, x - 1, rs)

printCell :: Tape -> IO ()
printCell (_, x, _) = putChar (toEnum x)

showTape :: Tape -> String
showTape (ls, x, rs) =
  "L: " ++ show (take 5 ls) ++
  " | C: " ++ show x ++
  " | R: " ++ show (take 5 rs)

run :: String -> Tape -> IO ()
run [] _ = return ()
run (c:cs) tape = do
  putStrLn $ "[instr: " ++ [c] ++ "] " ++ showTape tape
  case c of
    '>' -> run cs (moveRight tape)
    '<' -> run cs (moveLeft tape)
    '+' -> run cs (inc tape)
    '-' -> run cs (dec tape)
    '.' -> do
        putStrLn "[print]"
        printCell tape
        putStrLn "\n[done printing]"
        run cs tape
    _   -> run cs tape

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  putStrLn "[starting program]"
  run contents initialTape
  putStrLn "\n[Finished running]"
