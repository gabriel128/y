module Main where

import Data.Text (pack, unpack)
import Lib

main :: IO ()
main = do
  contents <- readFile "examples/ex1.yacll"
  case parseAndCompile (pack contents) of
    Left err -> putStrLn (unpack err)
    Right (_info, asmOutput) -> writeFile "examples/out/ex1.asm" (unpack asmOutput)
