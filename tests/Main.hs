import Test.Tasty
import Test.Tasty.HUnit

import IntCode

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "unit tests"
  [ testAdd
  , testMul
  , testInput
  , testOutput
  , testJumpIfTrue
  , testJumpIfFalse
  , testLessThan
  , testEqual
  ]

testAdd :: TestTree
testAdd = testGroup "addition tests"
  [ testAddAddress
  , testAddValue
  , testAddRelative
  , testAddMixed
  ]

testAddAddress :: TestTree
testAddAddress = testCase "addition with address parameter mode" $
  runProgram [] "1,4,3,0,99" @?= 99

testAddValue :: TestTree
testAddValue = testCase "addition with value parameter mode" $
  runProgram [] "1101,4,2,0,99" @?= 6

testAddRelative :: TestTree
testAddRelative = testCase "addition with relative parameter mode" $
  runProgram [] "109,1,22201,4,2,-1,99" @?= 3

testAddMixed :: TestTree
testAddMixed = testCase "addition with mixed parameter modes" $
  runProgram [] "1001,4,3,0,99" @?= 102

testMul :: TestTree
testMul = testGroup "multiplication tests"
  [ testMulAddress
  , testMulValue
  , testMulRelative
  ]

testMulAddress :: TestTree
testMulAddress = testCase "multiplication with address parameter mode" $
  let (mem, _) = run [] (parseMemory "2,4,2,0,99")
  in readAt (Address 0) mem @?= 198

testMulValue :: TestTree
testMulValue = testCase "multiplication with value parameter mode" $
  let (mem, _) = run [] (parseMemory "1102,4,2,0,99")
  in readAt (Address 0) mem @?= 8

testMulRelative :: TestTree
testMulRelative = testCase "multiplication with relative parameter mode" $
  let (mem, _) = run [] (parseMemory "109,1,22202,4,2,-1,99")
  in readAt (Address 0) mem @?= -4

testInput :: TestTree
testInput = testGroup "input tests"
  [ testInputAddress
  , testInputRelative
  ]

testInputAddress :: TestTree
testInputAddress = testCase "test input with address parameter mode" $
  runProgram [2] "3,0,1,0,0,0,99" @?= 4

testInputRelative :: TestTree
testInputRelative = testCase "test input with relative parameter mode" $
  runProgram [2] "109,1,203,0,1,1,1,0,99" @?= 4

testOutput :: TestTree
testOutput = testGroup "output tests"
  [ testOutputAddress
  , testOutputRelative
  ]

testOutputAddress :: TestTree
testOutputAddress = testCase "test output with address parameter mode" $
  runProgramOutput [] "4,0,99" @?= 4

testOutputRelative :: TestTree
testOutputRelative = testCase "test output with relative parameter mode" $
  runProgramOutput [] "109,1,204,0,99" @?= 1

testJumpIfTrue :: TestTree
testJumpIfTrue = testGroup "jump if true tests"
  [ testJumpIfTrueAddressT
  , testJumpIfTrueAddressF
  , testJumpIfTrueValueT
  , testJumpIfTrueValueF
  , testJumpIfTrueRelativeT
  , testJumpIfTrueRelativeF
  ]

testJumpIfTrueAddressT :: TestTree
testJumpIfTrueAddressT = testCase "test jump if true with address parameter mode, test true" $
  runProgram [] "5,0,0,99,99,1101,0,1,0,99" @?= 1

testJumpIfTrueAddressF :: TestTree
testJumpIfTrueAddressF = testCase "test jump if true with address parameter mode, test false" $
  runProgram [] "5,2,0,99,99,1101,0,1,0,99" @?= 5

testJumpIfTrueValueT :: TestTree
testJumpIfTrueValueT = testCase "test jump if true with value parameter mode, test true" $
  runProgram [] "1105,3,5,99,99,1101,0,1,0,99" @?= 1

testJumpIfTrueValueF :: TestTree
testJumpIfTrueValueF = testCase "test jump if true with value parameter mode, test false" $
  runProgram [] "1105,0,5,99,99,1101,0,1,0,99" @?= 1105

testJumpIfTrueRelativeT :: TestTree
testJumpIfTrueRelativeT = testCase "test jump if true with relative parameter mode, test true" $
  runProgram [] "109,1,2205,8,4,7,99,1101,0,1,0,99" @?= 1

testJumpIfTrueRelativeF :: TestTree
testJumpIfTrueRelativeF = testCase "test jump if true with relative parameter mode, test false" $
  runProgram [] "109,1,2205,7,4,99,99,1101,0,1,0,99" @?= 109

testJumpIfFalse :: TestTree
testJumpIfFalse = testGroup "jump if false tests"
  [ testJumpIfFalseAddressT
  , testJumpIfFalseAddressF
  , testJumpIfFalseValueT
  , testJumpIfFalseValueF
  , testJumpIfFalseRelativeT
  , testJumpIfFalseRelativeF
  ]

testJumpIfFalseAddressT :: TestTree
testJumpIfFalseAddressT = testCase "test jump if false with address parameter mode, test false" $
  runProgram [] "6,2,0,99,99,99,1101,0,1,0,99" @?= 1

testJumpIfFalseAddressF :: TestTree
testJumpIfFalseAddressF = testCase "test jump if false with address parameter mode, test true" $
  runProgram [] "6,0,0,99,99,1101,0,1,0,99" @?= 6

testJumpIfFalseValueT :: TestTree
testJumpIfFalseValueT = testCase "test jump if false with value parameter mode, test false" $
  runProgram [] "1106,0,5,99,99,1101,0,1,0,99" @?= 1

testJumpIfFalseValueF :: TestTree
testJumpIfFalseValueF = testCase "test jump if false with value parameter mode, test true" $
  runProgram [] "1106,3,5,99,99,1101,0,1,0,99" @?= 1106

testJumpIfFalseRelativeT :: TestTree
testJumpIfFalseRelativeT = testCase "test jump if false with relative parameter mode, test false" $
  runProgram [] "109,1,2206,7,4,7,99,1101,0,1,0,99" @?= 1

testJumpIfFalseRelativeF :: TestTree
testJumpIfFalseRelativeF = testCase "test jump if false with relative parameter mode, test true" $
  runProgram [] "109,1,2206,8,4,99,99,1101,0,1,0,99" @?= 109

testLessThan :: TestTree
testLessThan = testGroup "less than tests"
  [ testLessThanAddressT
  , testLessThanAddressF
  , testLessThanValueT
  , testLessThanValueF
  , testLessThanRelativeT
  , testLessThanRelativeF
  ]

testLessThanAddressT :: TestTree
testLessThanAddressT = testCase "test less than with address parameter mode, test true" $
  runProgram [] "7,1,0,0,99" @?= 1

testLessThanAddressF :: TestTree
testLessThanAddressF = testCase "test less than with address parameter mode, test false" $
  runProgram [] "7,0,1,0,99" @?= 0

testLessThanValueT :: TestTree
testLessThanValueT = testCase "test less than with value parameter mode, test true" $
  runProgram [] "1107,0,1,0,99" @?= 1

testLessThanValueF :: TestTree
testLessThanValueF = testCase "test less than with value parameter mode, test false" $
  runProgram [] "1107,1,0,0,99" @?= 0

testLessThanRelativeT :: TestTree
testLessThanRelativeT = testCase "test less than with relative parameter mode, test true" $
  runProgram [] "109,1,22207,0,1,-1,99" @?= 1

testLessThanRelativeF :: TestTree
testLessThanRelativeF = testCase "test less than with relative parameter mode, test false" $
  runProgram [] "109,1,22207,1,0,-1,99" @?= 0

testEqual :: TestTree
testEqual = testGroup "equal tests"
  [ testEqualAddressT
  , testEqualAddressF
  , testEqualValueT
  , testEqualValueF
  , testEqualRelativeT
  , testEqualRelativeF
  ]

testEqualAddressT :: TestTree
testEqualAddressT = testCase "test equal with address parameter mode, test true" $
  runProgram [] "8,0,5,0,99,8" @?= 1

testEqualAddressF :: TestTree
testEqualAddressF = testCase "test equal with address parameter mode, test false" $
  runProgram [] "8,0,1,0,99" @?= 0

testEqualValueT :: TestTree
testEqualValueT = testCase "test equal with value parameter mode, test true" $
  runProgram [] "1108,1,1,0,99" @?= 1

testEqualValueF :: TestTree
testEqualValueF = testCase "test equal with value parameter mode, test false" $
  runProgram [] "1108,1,0,0,99" @?= 0

testEqualRelativeT :: TestTree
testEqualRelativeT = testCase "test equal with relative parameter mode, test true" $
  runProgram [] "109,1,22208,0,6,-1,99,1" @?= 1

testEqualRelativeF :: TestTree
testEqualRelativeF = testCase "test equal with relative parameter mode, test false" $
  runProgram [] "109,1,22208,0,7,-1,99" @?= 0

runProgram :: [Integer] -> String -> Integer
runProgram is p =
  let (mem, _) = run is (parseMemory p)
  in readAt (Address 0) mem

runProgramOutput :: [Integer] -> String -> Integer
runProgramOutput is p =
  let (_, os) = run is (parseMemory p)
  in head os
