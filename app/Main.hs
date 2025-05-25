import Data.Char (isSpace) -- For more nuanced whitespace handling if needed
import Interpreter
import System.Environment (getArgs)
import System.IO (hFlush, readFile, stdout)
import Tape

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: bf-interpreter <file.bf>"
    (file : _) -> do
      contents_raw <- readFile file
      -- Filter to keep only actual Brainfuck commands.
      -- This helps with files that might have comments or other non-BF chars.
      let contents = filter (`elem` "><+-.,[]") contents_raw

      putStrLn "[starting program]\n"

      -- Run the interpreter, which now returns the accumulated output and a flag
      (programOutput, programDidPrint) <- run contents initialTape

      -- If the program produced any output via '.' commands
      if programDidPrint
        then do
          putStrLn "==== interpreted result ============"
          putStr programOutput -- Use putStr; programOutput might already have newlines
          hFlush stdout -- Ensure all output is flushed before the next line
          -- Ensure the shell prompt starts on a new line if the program output itself didn't end with one.
          if not (null programOutput) && last programOutput == '\n'
            then return () -- Output already ends with a newline
            else putStrLn "" -- Add a final newline for clean prompt separation
        else
          -- Optionally, indicate that no output was produced
          -- putStrLn "==== interpreted result ====\n(no output)"
          return ()

      putStrLn "\n[Finished program]\n"
