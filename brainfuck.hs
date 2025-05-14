import System.Environment (getArgs)
import System.IO (readFile)


main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  putStrLn "Loaded program: "
  putStrLn contents
