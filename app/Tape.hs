module Tape
  ( Tape,
    initialTape,
    moveRight,
    moveLeft,
    inc,
    dec,
    printCell,
    showTape,
    getCellValue,
    readChar
  )
where

type Tape = ([Int], Int, [Int])

initialTape :: Tape
initialTape = (repeat 0, 0, repeat 0)

moveRight :: Tape -> Tape
moveRight (ls, x, r : rs) = (x : ls, r, rs)
moveRight _ = error "moveRight: right list unexpectedly empty"

moveLeft :: Tape -> Tape
moveLeft (l : ls, x, rs) = (ls, l, x : rs)
moveLeft _ = error "moveLeft: left list unexpectedly empty"

inc :: Tape -> Tape
inc (ls, x, rs) = (ls, (x + 1) `mod` 256, rs)

dec :: Tape -> Tape
dec (ls, x, rs) = (ls, (x - 1 + 256) `mod` 256, rs)

printCell :: Tape -> IO ()
printCell (_, x, _) = putChar (toEnum x)

getCellValue :: Tape -> Int
getCellValue (_, val, _) = val

readChar :: Tape -> IO Tape
readChar (ls, _, rs) = do
  char <- getChar -- Read a single character from stdin
  return (ls, fromEnum char, rs) -- Convert char to its ASCII value and store it

showTape :: Tape -> String
showTape (ls, x, rs) =
  "L: "
    ++ show (take 5 ls)
    ++ " | C: "
    ++ show x
    ++ " | R: "
    ++ show (take 5 rs)
