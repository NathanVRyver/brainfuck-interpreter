-- src/Interpreter.hs
module Interpreter (run) where

import Tape (Tape, initialTape, moveRight, moveLeft, inc, dec, printCell, showTape, getCellValue)

-- this code is trash
extractLoopBody :: String -> (String, String)
extractLoopBody = go 0 ""
  where
    go _ acc [] = (acc, [])
    go 0 acc (']' : rest) = (acc, rest)
    go n acc ('[' : rest) =
      let (inner, after) = go (n + 1) "" rest
       in go (n - 1) (acc ++ "[" ++ inner ++ "]") after
    go n acc (x : xs) = go n (acc ++ [x]) xs

run :: String -> Tape -> IO ()
run [] _ = return ()
run (c : cs) tape = do
  putStrLn $ "[instr: " ++ [c] ++ "] (src: " ++ take 30 (c:cs) ++ ") " ++ showTape tape
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
    '[' -> do
      let (loop_body, after_loop) = extractLoopBody cs
      if getCellValue tape == 0
        then run after_loop tape
        else run (loop_body ++ (c:cs)) tape
    ']' -> run cs tape
    _ -> run cs tape
