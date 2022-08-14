{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module ParserTests (test_parser) where

import Ast.Ast
import Data.Either (isLeft)
import Parser.Parser
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Text.Megaparsec

-- import Test.Tasty.SmallCheck as SC

test_parser :: TestTree
test_parser = testGroup "Tests" [unitTests]

expr :: Expr
expr =
  let negEight = UnaryOp Neg (Const 8)
      ast1_1 = BinOp Add (Const 42) negEight
   in ast1_1

unitTests :: TestTree
unitTests =
  testGroup
    "parser tests"
    [ testCase "parse integers success" $
        parse parseInt "" "123" @?= Right (Const 123),
      --
      testCase "parse integers ignores comments afterwards" $
        parse (parseInt <* eof) "" "123 // hey you!" @?= Right (Const 123),
      --
      testCase "parse integers failure" $
        assertBool "" (isLeft $ parse parseInt "" "a123")
    ]
