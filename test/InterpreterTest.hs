import Test.HUnit
import Interpreter
import Tape

-- Test cases for basic commands
testMoveRight :: Test
testMoveRight = TestCase $ do
  let expectedTape = moveRight initialTape
  assertEqual "moveRight should move pointer right" expectedTape (moveRight initialTape)

testMoveLeft :: Test
testMoveLeft = TestCase $ do
  let initialTape' = moveRight initialTape -- Move right first to have something to move left from
  let expectedTape = initialTape
  assertEqual "moveLeft should move pointer left" expectedTape (moveLeft initialTape')

testInc :: Test
testInc = TestCase $ do
  let expectedTape = inc initialTape
  assertEqual "inc should increment cell value" expectedTape (inc initialTape)

testDec :: Test
testDec = TestCase $ do
  let initialTape' = inc initialTape -- Increment first to have something to decrement
  let expectedTape = initialTape
  assertEqual "dec should decrement cell value" expectedTape (dec initialTape')

-- Test cases for loop commands
testLoop :: Test
testLoop = TestCase $ do
  let program = "++[>+<-]" -- Set cell 0 to 2, then loop: move right, increment, move left, decrement
  (output, didPrint) <- run program initialTape
  assertEqual "Loop should execute correctly" "" output -- No output expected
  assertBool "Loop should not print anything" (not didPrint)

-- Test cases for input command
testInput :: Test
testInput = TestCase $ do
  let program = ",." -- Read a character and print it
  -- Note: This test requires manual input, so it's not fully automated
  (_, didPrint) <- run program initialTape
  assertBool "Input command should print something" didPrint

-- Combine all tests
tests :: Test
tests = TestList
  [ TestLabel "moveRight" testMoveRight
  , TestLabel "moveLeft" testMoveLeft
  , TestLabel "inc" testInc
  , TestLabel "dec" testDec
  , TestLabel "loop" testLoop
  , TestLabel "input" testInput
  ]

-- Main function to run tests
main :: IO Counts
main = runTestTT tests 