module Main where

import Control.Monad.Except (runExceptT)

import BCReader

asm :: String
asm = "define i64 @f(i64 %x) #0 {\n" ++
  "entry: \n" ++
  "%0 = add i64 %x, 1\n" ++
  "ret i64 %0" ++
  "}" 


main :: IO ()
main = print =<< runExceptT (readBC "test.bc")

