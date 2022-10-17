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
        parse parseExpr "" "123" @?= Right (Const 123),
      --
      testCase "parses negative integer success" $
        parse parseExpr "" "-123" @?= Right (UnaryOp Neg (Const 123)),
      --
      testCase "parses let stmts with const" $
        parse parseLet "" "let x =3;" @?= Right (Let "x" (Const 3)),
      --
      testCase "parses let stmts with var" $
        parse parseLet "" "let x = y;" @?= Right (Let "x" (Var "y")),
      --
      testCase "parses return stmt" $
        parse parseReturn "" "return y;" @?= Right (Return (Var "y")),
      --
      testCase "parses let stmt with sum" $
        parse parseLet "" "let x = 1 + y;" @?= Right (Let "x" (BinOp Add (Const 1) (Var "y"))),
      --
      testCase "parses sum stmts" $
        parse (parseExpr <* eof) "" "(1 + 3) - 2" @?= Right (BinOp Sub (BinOp Add (Const 1) (Const 3)) (Const 2)),
      --
      testCase "parses mult-sums stmts" $
        parse (parseExpr <* eof) "" "1 * 3 + 2" @?= Right (BinOp Add (BinOp Mul (Const 1) (Const 3)) (Const 2)),
      --
      testCase "parses let stmts 1" $
        parse parseLet "" "let y = -30;" @?= Right (Let "y" (UnaryOp Neg (Const 30))),
      --
      testCase "parse integers ignores comments afterwards" $
        parse (parseExpr <* eof) "" "123 // hey you!" @?= Right (Const 123),
      --
      testCase "parse integers failure" $
        assertBool "" (isLeft $ parse parseUint "" "a123"),
      --
      testCase "parse program" $
        runProgramParser "     let x = 3; let y = 4; return (x + y);"
          @?= Right (Program [Let "x" (Const 3), Let "y" (Const 4), Return (BinOp Add (Var "x") (Var "y"))]),
      --
      testCase "parse invalid program fails" $
        let res = runProgramParser "let x = 3 \nlet y = 4;"
         in assertBool "" (isLeft res)
    ]
