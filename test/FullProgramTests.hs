module FullProgramTests (test_full_progs) where

import Data.List (isInfixOf)
import System.Process (readProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

test_full_progs :: TestTree
test_full_progs = testGroup "Tests" [unitTests]

compileFile :: String -> String -> IO String
compileFile inFile outFile =
  readProcess "/home/gabriel/.local/bin/stack" ["run", "--", "-i", inFile, "-o", outFile] []

runFile :: String -> IO String
runFile file = readProcess file [] []

compileAndRun :: String -> IO String
compileAndRun progName = do
  compRes <- compileFile ("./examples/" <> progName <> ".yacll") ("./test/results/" <> progName)
  assertBool compRes ("Compilation successful" `isInfixOf` compRes)
  runRes <- runFile ("/home/gabriel/dev/workspaces/yacll/test/results/" <> progName)
  pure runRes

unitTests :: TestTree
unitTests =
  testGroup
    "parser tests"
    [ testCase "arithm" $ do
        runRes <- compileAndRun "arithm"
        assertEqual "" "8\n" runRes,
      testCase "arithm2" $ do
        runRes <- compileAndRun "arithm2"
        assertEqual "" "109\n6\n" runRes,
      testCase "Compilation error" $ do
        compRes <- compileFile "./examples/comp_err.yacll" "./test/results/comp_err"
        assertBool compRes ("expecting identifier or white space" `isInfixOf` compRes),
      testCase "Compilation error 2" $ do
        compRes <- compileFile "./examples/comp_err2.yacll" "./test/results/comp_err"
        assertBool compRes ("unexpected ')" `isInfixOf` compRes)
    ]
