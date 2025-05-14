-- src/Interpreter.hs
module Interpreter (run) where

import Tape (Tape, initialTape, moveRight, moveLeft, inc, dec, printCell, showTape, getCellValue)

-- Corrected extractLoopBody function
extractLoopBody :: String -> (String, String)
extractLoopBody s = go 1 "" s
  where
    -- level: current nesting level. Starts at 1 because we are already inside one '['.
    -- current_loop_body: accumulator for the body of the current loop.
    -- remaining_code: the rest of the string to process.
    go :: Int -> String -> String -> (String, String)
    go _ current_loop_body [] = (current_loop_body, []) -- Unbalanced bracket, loop body is till end of string
    go 1 current_loop_body (']':rs) = (current_loop_body, rs) -- Found matching ']' for the initial loop
    go level current_loop_body (']':rs) = go (level - 1) (current_loop_body ++ [']']) rs -- Closing a nested ']'
    go level current_loop_body ('[':rs) = go (level + 1) (current_loop_body ++ ['[']) rs -- Opening a nested '['
    go level current_loop_body (c_char:rs)   = go level (current_loop_body ++ [c_char]) rs -- Other char

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
