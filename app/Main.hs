{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

-- import System.Console.CmdArgs

-- import System.Console.CmdArgs
-- import System.Console.CmdArgs

-- import System.Console.CmdArgs

import Control.Arrow (left)
import Control.Monad.Except (MonadError, MonadIO, liftEither, runExceptT)
import Data.Text (pack, unpack)
import Lib
import Mtl

main :: (MonadIO m, MonadPrinter m) => m ()
main = do
  let file = "./examples/ex1.yacll"
  let outFile = "./examples/out/ex1"
  res <- runExceptT $ runCompiler file outFile
  case res of
    Right _ -> printf "Compilation successful"
    Left err -> printf err

runCompiler :: (MonadIO m, MonadProcess m, MonadPrinter m, MonadFiles m, MonadError String m) => String -> String -> m ()
runCompiler yacllFile outFile = do
  let asmFile = outFile <> ".asm"
  let objFile = outFile <> ".o"
  contents <- readFromFile yacllFile

  -- Compile!
  (_info, asmOutput) <- liftEither . left unpack $ parseAndCompile (pack contents)
  writeToFile asmFile (unpack asmOutput)

  printf "Compiling asm..."
  out <- readFromProcess "nasm" ["-f", "elf64", asmFile] []

  printf $ "Asm Finished " <> out
  printf "Linking..."

  lOut <- readFromProcess "gcc" [objFile, "-no-pie", "-z", "noexecstack", "-o", outFile] []
  printf $ "Linking Finished" <> lOut

replaceYacllExt :: String -> String -> Either String String
replaceYacllExt yacllFile newExt = go . reverse $ yacllFile
  where
    go ('l' : 'l' : 'c' : 'a' : 'y' : '.' : rest) = Right (reverse rest <> newExt)
    go _ = Left "Extension it's not yacll match"
