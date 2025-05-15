-- src/Interpreter.hs
module Interpreter (run) where

import Tape (Tape, initialTape, moveRight, moveLeft, inc, dec, printCell, showTape, getCellValue, readChar) -- printCell will no longer be used directly here for output

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

-- run now returns (accumulatedOutputString, wasAnythingPrintedByDot)
run :: String -> Tape -> IO (String, Bool)
run [] _ = return ("", False) -- Base case: no input string, so no output, nothing printed
run (c : cs) tape = do
  -- Diagnostic log remains
  putStrLn $ "[instr: " ++ [c] ++ "] (src: " ++ take 30 (c:cs) ++ ") " ++ showTape tape
  case c of
    '>' -> run cs (moveRight tape)
    '<' -> run cs (moveLeft tape)
    '+' -> run cs (inc tape)
    '-' -> run cs (dec tape)
    '.' -> do
      let char_to_print = toEnum (getCellValue tape) :: Char
      (recursive_output, _) <- run cs tape -- We don't need recursive_did_print directly, this path itself is a print
      return (char_to_print : recursive_output, True) -- Prepend current char, mark True as this path printed
    ',' -> do -- Handle the input command
      newTape <- readChar tape -- Read a char and update the tape
      run cs newTape -- Continue execution with the updated tape
    '[' -> do
      let (loop_body, after_loop) = extractLoopBody cs
      if getCellValue tape == 0
        then run after_loop tape -- Propagate result from after_loop
        else run (loop_body ++ (c:cs)) tape -- Propagate result from loop execution
    ']' -> run cs tape -- Propagate result
    _   -> run cs tape -- Ignore non-command chars, propagate result
