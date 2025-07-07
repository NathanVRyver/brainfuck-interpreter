import Test.HUnit
import Interpreter
import Tape

-- Test cases for basic commands
testMoveRight :: Test
testMoveRight = TestCase $ do
  let tape1 = moveRight initialTape
  let tape2 = moveRight tape1
  assertEqual "moveRight should move pointer right" 
    (getCellValue tape2) (getCellValue tape2)

testMoveLeft :: Test
testMoveLeft = TestCase $ do
  let tape1 = moveRight initialTape
  let tape2 = moveLeft tape1
  assertEqual "moveLeft should move pointer left" 
    (getCellValue initialTape) (getCellValue tape2)

testInc :: Test
testInc = TestCase $ do
  let tape1 = inc initialTape
  assertEqual "inc should increment cell value" 1 (getCellValue tape1)

testDec :: Test
testDec = TestCase $ do
  let tape1 = inc initialTape
  let tape2 = dec tape1
  assertEqual "dec should decrement cell value" 0 (getCellValue tape2)

testWrapping :: Test
testWrapping = TestCase $ do
  let tape255 = iterate inc initialTape !! 255
  let tapeWrapped = inc tape255
  assertEqual "inc should wrap from 255 to 0" 0 (getCellValue tapeWrapped)
  
  let tapeDecWrapped = dec initialTape
  assertEqual "dec should wrap from 0 to 255" 255 (getCellValue tapeDecWrapped)

-- Test cases for simple output
testOutput :: Test
testOutput = TestCase $ do
  let program = "+++++++++."  -- ASCII 9 (tab)
  (output, didPrint) <- runSilent program initialTape
  assertEqual "Output should be correct" "\t" output
  assertBool "Should indicate printing occurred" didPrint

-- Test cases for loop commands
testLoop :: Test
testLoop = TestCase $ do
  let program = "++[>+<-]"  -- Set cell 0 to 2, then loop: move right, increment, move left, decrement
  (output, didPrint) <- runSilent program initialTape
  assertEqual "Loop should execute correctly" "" output
  assertBool "Loop should not print anything" (not didPrint)

-- Test loop with output
testLoopWithOutput :: Test
testLoopWithOutput = TestCase $ do
  let program = "+++[>++++++++++<-]>."  -- 3 * 10 = 30, print space
  (output, didPrint) <- runSilent program initialTape
  assertEqual "Loop output should be correct" "\RS" output  -- ASCII 30
  assertBool "Loop should print something" didPrint

-- Test nested loops
testNestedLoop :: Test
testNestedLoop = TestCase $ do
  let program = "++[>+[>+<-]<-]"  -- 2 * 1 = 2 in cell 2
  (output, didPrint) <- runSilent program initialTape
  assertEqual "Nested loop should execute correctly" "" output
  assertBool "Nested loop should not print anything" (not didPrint)

-- Test empty loop (should be skipped)
testEmptyLoop :: Test
testEmptyLoop = TestCase $ do
  let program = "[>+<]"  -- Should skip because cell 0 is 0
  (output, didPrint) <- runSilent program initialTape
  assertEqual "Empty loop should be skipped" "" output
  assertBool "Empty loop should not print anything" (not didPrint)

-- Test hello world-like pattern
testHelloWorld :: Test
testHelloWorld = TestCase $ do
  let program = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
  (output, didPrint) <- runSilent program initialTape
  assertEqual "Hello World should work" "Hello World!\n" output
  assertBool "Hello World should print" didPrint

-- Test clear cell idiom
testClearCell :: Test
testClearCell = TestCase $ do
  let program = "+++++[-]"  -- Set cell to 5, then clear it
  (output, didPrint) <- runSilent program initialTape
  assertEqual "Clear cell should produce no output" "" output
  assertBool "Clear cell should not print" (not didPrint)

-- Combine all tests
tests :: Test
tests = TestList
  [ TestLabel "moveRight" testMoveRight
  , TestLabel "moveLeft" testMoveLeft
  , TestLabel "inc" testInc
  , TestLabel "dec" testDec
  , TestLabel "wrapping" testWrapping
  , TestLabel "output" testOutput
  , TestLabel "loop" testLoop
  , TestLabel "loopWithOutput" testLoopWithOutput
  , TestLabel "nestedLoop" testNestedLoop
  , TestLabel "emptyLoop" testEmptyLoop
  , TestLabel "helloWorld" testHelloWorld
  , TestLabel "clearCell" testClearCell
  ]

-- Main function to run tests
main :: IO Counts
main = runTestTT tests 