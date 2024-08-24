{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module ParserTests (test_parser) where

import Ast.Ast
import Data.Either (isLeft)
import Parser.Parser
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Text.Megaparsec
import Types.Defs
import Types.Defs (Type(TyToInfer))

-- import Test.Tasty.SmallCheck as SC

test_parser :: TestTree
test_parser = testGroup "Tests" [unitTests]

expr :: Expr
expr =
  let negEight = UnaryOp Neg (Const TyInt 8)
      ast1_1 = BinOp Add (Const TyInt 42) negEight
   in ast1_1

unitTests :: TestTree
unitTests =
  testGroup
    "parser tests"
    [ testCase "parse integers success" $
        parse parseExpr "" "123" @?= Right (Const TyInt 123),
      --
      testCase "parses negative integer success" $
        parse parseExpr "" "-123" @?= Right (UnaryOp Neg (Const TyInt 123)),
      --
      testCase "parses let stmts with const" $
        parse parseLet "" "let x : int = 3;" @?= Right (Let (Native TyInt) "x" (Const TyInt 3)),
      --
      testCase "parses let stmts with var" $
        parse parseLet "" "let x:int = y;" @?= Right (Let (Native TyInt) "x" (Var TyToInfer "y")),
      --
      testCase "parses return stmt" $
        parse parseReturn "" "return y;" @?= Right (Return (Var TyToInfer "y")),
      --
      testCase "parses let stmt with sum" $
        parse parseLet "" "let x: int = 1 + y;" @?= Right (Let (Native TyInt) "x" (BinOp Add (Const TyInt 1) (Var TyToInfer "y"))),
      --
      testCase "parses sum stmts" $
        parse (parseExpr <* eof) "" "(1 + 3) - 2" @?= Right (BinOp Sub (BinOp Add (Const TyInt 1) (Const TyInt 3)) (Const TyInt 2)),
      --
      testCase "parses mult-sums stmts" $
        parse (parseExpr <* eof) "" "1 * 3 + 2" @?= Right (BinOp Add (BinOp Mul (Const TyInt 1) (Const TyInt 3)) (Const TyInt 2)),
      --
      testCase "parses shift left with vars" $
        parse (parseExpr <* eof) "" "y << 2" @?= Right (BinOp ShiftL (Var TyToInfer "y") (Const TyInt 2)),
      --
      testCase "parses shift left with num" $
        parse (parseExpr <* eof) "" "1 << 2" @?= Right (BinOp ShiftL (Const TyInt 1) (Const TyInt 2)),
      --
      testCase "parses let stmts 1" $
        parse parseLet "" "let y:   int = -30;" @?= Right (Let (Native TyInt) "y" (UnaryOp Neg (Const TyInt 30))),
      --
      testCase "parse integers ignores comments afterwards" $
        parse (parseExpr <* eof) "" "123 // hey you!" @?= Right (Const TyInt 123),
      --
      testCase "parse integers failure" $
        assertBool "" (isLeft $ parse parseUint "" "a123"),
      --
      testCase "parse program" $
        runProgramParser "     let x : int = 3; let y : int = 4; return (x + y);"
          @?= Right (Program [Let (Native TyInt) "x" (Const TyInt 3), Let (Native TyInt) "y" (Const TyInt 4), Return (BinOp Add (Var TyToInfer "x") (Var TyToInfer "y"))]),
      --
      testCase "parse invalid program fails" $
        let res = runProgramParser "let x: int = 3 \nlet y: int = 4;"
         in assertBool "" (isLeft res)
    ]
