import Interpreter
import System.Environment (getArgs)
import System.IO (hFlush, stdout, stderr, hPutStrLn)
import System.Exit (exitFailure)
import Control.Exception (catch, SomeException)
import Tape

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    ["--help"] -> usage
    ["-h"] -> usage
    ["--version"] -> putStrLn "bf-interpreter version 0.1.0.0"
    ["--silent", file] -> runBF file True
    [file] -> runBF file False
    _ -> usage

usage :: IO ()
usage = do
  putStrLn "Usage: bf-interpreter [OPTIONS] <file.bf>"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  --silent    Run without diagnostic output"
  putStrLn "  --help, -h  Show this help message"
  putStrLn "  --version   Show version information"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  bf-interpreter examples/hello_world.bf"
  putStrLn "  bf-interpreter --silent examples/hello_world.bf"

runBF :: String -> Bool -> IO ()
runBF file silent = do
  result <- catch (Prelude.readFile file) $ \(e :: SomeException) -> do
    hPutStrLn stderr $ "Error reading file '" ++ file ++ "': " ++ show e
    exitFailure
  
  let contents = filter (`elem` "><+-.,[]") result
  
  if null contents
    then do
      hPutStrLn stderr "Warning: No valid Brainfuck commands found in file"
      return ()
    else do
      unless silent $ putStrLn "[Starting program]\n"
      
      let runFunction = if silent then runSilent else run
      (programOutput, programDidPrint) <- runFunction contents initialTape
      
      if programDidPrint
        then do
          unless silent $ putStrLn "==== Interpreted result ============"
          putStr programOutput
          hFlush stdout
          unless (not (null programOutput) && last programOutput == '\n') $
            putStrLn ""
        else unless silent $ putStrLn "==== Interpreted result ====\n(no output)"
      
      unless silent $ putStrLn "\n[Finished program]\n"

unless :: Bool -> IO () -> IO ()
unless True _ = return ()
unless False action = action
