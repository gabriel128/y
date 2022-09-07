module Main where

import Data.Text (pack, unpack)
import Lib
import System.Process

main :: IO ()
main = do
  let file = "./examples/ex1.yacll"
  let outFile = "./examples/out/ex1.asm"
  let objFile = "./examples/out/ex1.o"
  contents <- readFile file
  case parseAndCompile (pack contents) of
    Left err -> putStrLn (unpack err)
    Right (_info, asmOutput) -> writeFile outFile (unpack asmOutput)
  putStrLn "Compiling asm..."
  out <- readProcess "nasm" ["-f", "elf64", outFile] []
  putStrLn $ "Asm Finished " <> out
  putStrLn "Linking..."
  lOut <- readProcess "gcc" [objFile, "-no-pie", "-z", "noexecstack"] []
  putStrLn $ "Linking Finished" <> lOut
