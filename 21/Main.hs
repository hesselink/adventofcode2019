{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import Data.Char (ord, chr)

import IntCode

main :: IO ()
main = do
  program <- memoryFromInputFile "21"
  -- written by hand:
  -- jump if c has a hole and d doesn't
  -- otherwise jump if b has a hole and d doesn't
  -- otherwise jump if a has a hole
  let script = map (fromIntegral . ord) ("NOT C J\nAND D J\nNOT B T\nAND D T\nOR T J\nNOT A T\nOR T J\nWALK\n")
      os = exec script program
  print (last os)
  -- written by hand:
  -- same as above, except on the first check:
  -- also make sure that if you have to jump again (e has a hole) you
  -- can land on h
  let script2 = map (fromIntegral . ord) ("NOT C J\nAND D J\nOR E T\nOR H T\nAND T J\nNOT B T\nAND D T\nOR T J\nNOT A T\nOR T J\nRUN\n")
      os2 = exec script2 program
  print (last os2)

printOutput :: [Integer] -> IO ()
printOutput = putStrLn . map (chr . fromIntegral)
