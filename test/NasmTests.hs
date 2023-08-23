module NasmTests (test_nasm) where

import Data.Text (Text)
import qualified Data.Text as T
import Nasm.Data
import Nasm.Data (MemDeref (Deref))
import Nasm.Dsl
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Utils

test_nasm :: TestTree
test_nasm = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "NasmInstr"
    [ testCase "mov reg, reg" $ do assertEqual "" (textPrint $ Mov Rbp Rsp) "mov rbp, rsp",
      testCase "mov reg, mem" $ do assertEqual "" (textPrint $ Mov Rbp (Deref Rbp 8)) "mov rbp, [rbp+8]",
      testCase "mov reg, mem" $ do assertEqual "" (textPrint $ Mov Rbp (Deref Rbp (-8))) "mov rbp, [rbp-8]",
      testCase "mov mem, reg" $ do assertEqual "" (textPrint $ Mov (Deref Rbp 8) Rax) "mov [rbp+8], rax",
      testCase "mov mem, reg" $ do assertEqual "" (textPrint $ Mov (Deref Rbp 8) (3 :: Int)) "mov [rbp+8], 3",
      -- testCase "mov mem, reg" $ do assertEqual "" (textPrint $ Mov (Deref Rbp 8) (Deref Rbp 8)) "mov [rbp+8], 3",
      testCase "mov mem, label" $ do assertEqual "" (textPrint $ Mov Rax ("blah" :: Text)) "mov rax, blah",
      testCase "neg mem" $ do assertEqual "" (textPrint $ Neg' (Deref Rax 8)) "neg [rax+8]",
      -- testCase "neg mem" $ do assertEqual "" (textPrint $ Neg' (8 :: Int)) "neg [rax+8]",
      testCase "neg reg" $ do assertEqual "" (textPrint $ Neg' Rax) "neg rax"
    ]
