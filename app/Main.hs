{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

-- import System.Console.CmdArgs

-- import System.Console.CmdArgs
-- import System.Console.CmdArgs

-- import System.Console.CmdArgs

import Control.Arrow (left)
import Control.Monad.Except (ExceptT, liftEither, liftIO, runExceptT)
import Data.Text (pack, unpack)
import Lib
import System.Process

main :: IO ()
main = do
  let file = "./examples/ex1.yacll"
  let outFile = "./examples/out/ex1"
  res <- runExceptT $ runCompiler file outFile
  case res of
    Right _ -> putStrLn "Compilation successful"
    Left err -> putStrLn err
  pure ()

runCompiler :: String -> String -> ExceptT String IO ()
runCompiler yacllFile outFile = do
  let asmFile = outFile <> ".asm"
  let objFile = outFile <> ".o"
  contents <- liftIO $ readFile yacllFile

  -- Compile!
  (_info, asmOutput) <- liftEither . left unpack $ parseAndCompile (pack contents)
  liftIO $ writeFile asmFile (unpack asmOutput)

  liftIO $ putStrLn "Compiling asm..."
  out <- liftIO $ readProcess "nasm" ["-f", "elf64", asmFile] []

  liftIO $ putStrLn $ "Asm Finished " <> out
  liftIO $ putStrLn "Linking..."

  lOut <- liftIO $ readProcess "gcc" [objFile, "-no-pie", "-z", "noexecstack", "-o", outFile] []
  liftIO $ putStrLn $ "Linking Finished" <> lOut

replaceYacllExt :: String -> String -> Either String String
replaceYacllExt yacllFile newExt = go . reverse $ yacllFile
  where
    go ('l' : 'l' : 'c' : 'a' : 'y' : '.' : rest) = Right (reverse rest <> newExt)
    go _ = Left "Extension it's not yacll match"
