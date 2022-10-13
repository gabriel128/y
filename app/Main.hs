{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

-- import System.Console.CmdArgs

import Control.Arrow (left)
import Data.Text (pack, unpack)
import Lib
import LocalMtl
import System.Console.CmdArgs

data Args = Args {outputFile :: String, inputFile :: String}
  deriving (Show, Data, Typeable)

consoleArgs :: Args
consoleArgs = Args {outputFile = def, inputFile = def}

main :: (MonadTIO m, MonadPrinter m) => m ()
main = do
  parsedArgs <- liftTIO $ cmdArgs consoleArgs
  printLn (show parsedArgs)

  let inFile = inputFile parsedArgs
  let outFile = outputFile parsedArgs

  res <- runEitherT $ runCompiler inFile outFile
  case res of
    Right _ -> printLn "Compilation successful"
    Left err -> printLn err

runCompiler ::
  ( MonadProcess m,
    MonadTIO m,
    MonadPrinter m,
    MonadFiles m,
    MonadEither String m
  ) =>
  String ->
  String ->
  m ()
runCompiler yacllFile outFile = do
  let asmFile = outFile <> ".asm"
  let objFile = outFile <> ".o"
  contents <- readFromFile yacllFile

  liftTIO $ putStrLn "blah"
  -- Compile!
  (_info, asmOutput) <- liftEither . left unpack $ parseAndCompile (pack contents)
  writeToFile asmFile (unpack asmOutput)

  printLn "Compiling asm..."
  out <- readFromProcess "nasm" ["-f", "elf64", asmFile] []

  printLn $ "Asm Finished " <> out
  printLn "Linking..."

  lOut <- readFromProcess "gcc" [objFile, "-no-pie", "-z", "noexecstack", "-o", outFile] []
  printLn $ "Linking Finished" <> lOut

replaceYacllExt :: String -> String -> Either String String
replaceYacllExt yacllFile newExt = go . reverse $ yacllFile
  where
    go ('l' : 'l' : 'c' : 'a' : 'y' : '.' : rest) = Right (reverse rest <> newExt)
    go _ = Left "Extension it's not yacll match"
