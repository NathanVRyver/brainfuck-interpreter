{-# LANGUAGE ScopedTypeVariables #-}
module Interpreter (run, runSilent) where

import Tape (Tape, moveRight, moveLeft, inc, dec, showTape, getCellValue, readChar)
import Control.Exception (catch, SomeException)

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
run = runWithLogging True

-- runSilent executes without diagnostic logging
runSilent :: String -> Tape -> IO (String, Bool)
runSilent = runWithLogging False

-- Internal function that handles both verbose and silent execution
runWithLogging :: Bool -> String -> Tape -> IO (String, Bool)
runWithLogging _ [] _ = return ("", False)
runWithLogging verbose (c : cs) tape = do
  -- Diagnostic log only if verbose mode is enabled
  when verbose $
    putStrLn $ "[instr: " ++ [c] ++ "] (src: " ++ take 30 (c:cs) ++ ") " ++ showTape tape
  
  case c of
    '>' -> runWithLogging verbose cs (moveRight tape)
    '<' -> runWithLogging verbose cs (moveLeft tape)
    '+' -> runWithLogging verbose cs (inc tape)
    '-' -> runWithLogging verbose cs (dec tape)
    '.' -> do
      let char_to_print = toEnum (getCellValue tape) :: Char
      (recursive_output, _) <- runWithLogging verbose cs tape
      return (char_to_print : recursive_output, True)
    ',' -> do
      let (leftTape, _, rightTape) = tape
      newTape <- readChar tape `catch` \(_ :: SomeException) -> do
        putStrLn "Error reading input, using 0"
        return (leftTape, 0, rightTape)
      runWithLogging verbose cs newTape
    '[' -> do
      let (loop_body, after_loop) = extractLoopBody cs
      if getCellValue tape == 0
        then runWithLogging verbose after_loop tape
        else runWithLogging verbose (loop_body ++ (c:cs)) tape
    ']' -> runWithLogging verbose cs tape
    _   -> runWithLogging verbose cs tape

-- Helper function for when
when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()