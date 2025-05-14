import Interpreter
import System.Environment (getArgs)
import System.IO (readFile)
import Tape

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: horsing-around <file.bf>"
    (file : _) -> do
      contents <- readFile file
      putStrLn "[starting program]\n"
      run contents initialTape
      putStrLn "\n[Finished program]\n"
