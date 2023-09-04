module NasmTests (test_nasm) where

import Data.Text (Text)
import Nasm.Data
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
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
      testCase "mov mem, reg" $ do assertEqual "" (textPrint $ Mov (Deref Rbp 8) Rax) "mov qword [rbp+8], rax",
      testCase "mov mem, reg" $ do assertEqual "" (textPrint $ Mov (Deref Rbp 8) (3 :: Int)) "mov qword [rbp+8], 3",
      -- testCase "mov mem, reg" $ do assertEqual "" (textPrint $ Mov (Deref Rbp 8) (Deref Rbp 8)) "mov [rbp+8], 3",
      testCase "sub mem, 16" $ do assertEqual "" (textPrint $ Sub (Deref Rbp 8) (16 :: Int)) "sub qword [rbp+8], 16",
      testCase "sub reg, 16" $ do assertEqual "" (textPrint $ Sub Rbp (16 :: Int)) "sub rbp, 16",
      testCase "add mem, 16" $ do assertEqual "" (textPrint $ Add (Deref Rbp 8) (16 :: Int)) "add qword [rbp+8], 16",
      testCase "add reg, 16" $ do assertEqual "" (textPrint $ Add Rbp (16 :: Int)) "add rbp, 16",
      testCase "mov mem, label" $ do assertEqual "" (textPrint $ Mov Rax ("blah" :: Text)) "mov rax, blah",
      testCase "neg mem" $ do assertEqual "" (textPrint $ Neg (Deref Rax 8)) "neg [rax+8]",
      -- testCase "neg mem" $ do assertEqual "" (textPrint $ Neg (8 :: Int)) "neg [rax+8]",
      testCase "neg reg" $ do assertEqual "" (textPrint $ Neg Rax) "neg rax",
      testCase "push 3" $ do assertEqual "" (textPrint $ Push (3 :: Int)) "push 3",
      testCase "push [rsp]" $ do assertEqual "" (textPrint $ Push (Deref Rbp 0)) "push [rbp]",
      testCase "push rsp" $ do assertEqual "" (textPrint $ Push Rax) "push rax",
      testCase "pop 3" $ do assertEqual "" (textPrint $ Pop (3 :: Int)) "pop 3",
      testCase "pop [rsp]" $ do assertEqual "" (textPrint $ Pop (Deref Rbp 0)) "pop [rbp]",
      testCase "pop rsp" $ do assertEqual "" (textPrint $ Pop Rax) "pop rax"
    ]
