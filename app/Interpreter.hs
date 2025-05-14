-- src/Interpreter.hs
module Interpreter (run) where

import Tape

run :: String -> Tape -> IO ()
run [] _ = return ()
run (c : cs) tape = do
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
    _ -> run cs tape
