module FullProgramTests (test_full_progs) where

import Data.List (isInfixOf)
import System.Process (readProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

test_full_progs :: TestTree
test_full_progs = testGroup "Example tests" [unitTests]

compileFile :: String -> String -> IO String
compileFile inFile outFile =
  readProcess "stack" ["run", "--", "-i", inFile, "-o", outFile] []

runFile :: String -> IO String
runFile file = readProcess file [] []

compileAndRun :: String -> IO String
compileAndRun progName = do
  compRes <- compileFile ("./examples/" <> progName <> ".y") ("./test/results/" <> progName)
  assertBool compRes ("Compilation successful" `isInfixOf` compRes)
  runRes <- runFile ("./test/results/" <> progName)
  pure runRes

unitTests :: TestTree
unitTests =
  testGroup
    "Full programs tests"
    [ testCase "arithm" $ do
        runRes <- compileAndRun "arithm"
        assertEqual "" "8\n" runRes
        -- testCase "arithm2" $ do
        --   runRes <- compileAndRun "arithm2"
        --   assertEqual "" "109\n6\n" runRes,
        -- testCase "simple sum" $ do
        --   runRes <- compileAndRun "simple_sum"
        --   assertEqual "" "3\n" runRes,
        -- testCase "redef" $ do
        --   runRes <- compileAndRun "redef"
        --   assertEqual "" "17\n" runRes,
        -- testCase "Compilation error, unbound vars" $ do
        --   compRes <- compileFile "./examples/unbound.y" "./test/results/unbound"
        --   assertBool compRes ("Var not bound: x" `isInfixOf` compRes),
        -- testCase "Compilation error" $ do
        --   compRes <- compileFile "./examples/comp_err.y" "./test/results/comp_err"
        --   assertBool compRes ("expecting identifier or white space" `isInfixOf` compRes),
        -- testCase "Compilation error 2" $ do
        --   compRes <- compileFile "./examples/comp_err2.y" "./test/results/comp_err"
        --   assertBool compRes ("unexpected ')" `isInfixOf` compRes)
    ]
