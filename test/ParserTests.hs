{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module ParserTests (test_parser) where

import Ast.Ast
import Ast.TypedAst
import Data.Either (isLeft)
import Parser.Parser
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Text.Megaparsec
import Types.Defs

-- import Test.Tasty.SmallCheck as SC

test_parser :: TestTree
test_parser = testGroup "Parser Tests" unitTests

expr :: TypedExpr
expr =
  let negEight = TUnaryOp TyToInfer Neg (TConst (mkImmNativeType I64) (NativeInt 8))
      ast1_1 = TBinOp TyToInfer Add (TConst (mkImmNativeType I64) (NativeInt 42)) negEight
   in ast1_1

unitTests :: [TestTree]
unitTests =
  [ testCase "parse integers success" $
      parse parseExpr "" "123" @?= Right (TConst (mkImmNativeType U64) (NativeInt 123)),
    --
    testCase "parses negative integer success" $
      parse parseExpr "" "-123" @?= Right (TUnaryOp (mkImmNativeType I64) Neg (TConst (mkImmNativeType I64) (NativeInt 123))),
    --
    testCase "parses let stmts with const" $
      parse parseLet "" "x : u64 = 3;" @?= Right (TLet (mkImmNativeType U64) "x" (TConst (mkImmNativeType U64) (NativeInt 3))),
    --
    testCase "parses let stmts with mut" $
      parse parseLet "" "x: mut u64 = 3;" @?= Right (TLet (mkMutNativeType U64) "x" (TConst (mkImmNativeType U64) (NativeInt 3))),
    --
    testCase "parses let stmts with var" $
      parse parseLet "" "x:i64 = y;" @?= Right (TLet (mkImmNativeType I64) "x" (TVar TyToInfer "y")),
    --
    testCase "parses return stmt" $
      parse parseReturn "" "return y;" @?= Right (TReturn TyToInfer (TVar TyToInfer "y")),
    --
    testCase "parses let stmt with sum" $
      parse parseLet "" "x: u64 = 1 + y;"
        @?= Right
          ( TLet
              (mkImmNativeType U64)
              "x"
              (TBinOp TyToInfer Add (TConst (mkImmNativeType U64) (NativeInt 1)) (TVar TyToInfer "y"))
          ),
    --
    testCase "parses let stmt with inference" $
      parse parseLetToInfer "" "x = 1 + 3;"
        @?= Right
          ( TLet
              TyToInfer
              "x"
              (TBinOp TyToInfer Add (TConst (mkImmNativeType U64) (NativeInt 1)) (TConst (mkImmNativeType U64) (NativeInt 3)))
          ),
    ---
    testCase "parses sum exprs" $
      parse (parseExpr <* eof) "" "(1 + 3) - 2"
        @?= Right
          ( TBinOp
              TyToInfer
              Sub
              (TBinOp TyToInfer Add (TConst (mkImmNativeType U64) (NativeInt 1)) (TConst (mkImmNativeType U64) (NativeInt 3)))
              (TConst (mkImmNativeType U64) (NativeInt 2))
          ),
    --
    testCase "parses mult-sums stmts" $
      parse (parseExpr <* eof) "" "1 * 3 + 2"
        @?= Right
          ( TBinOp
              TyToInfer
              Add
              (TBinOp TyToInfer Mul (TConst (mkImmNativeType U64) (NativeInt 1)) (TConst (mkImmNativeType U64) (NativeInt 3)))
              (TConst (mkImmNativeType U64) (NativeInt 2))
          ),
    -- --
    testCase "parses shift left with vars" $
      parse (parseExpr <* eof) "" "y << 2" @?= Right (TBinOp TyToInfer ShiftL (TVar TyToInfer "y") (TConst (mkImmNativeType U64) (NativeInt 2))),
    -- --
    testCase "parses shift left with num" $
      parse (parseExpr <* eof) "" "1 << 2"
        @?= Right
          ( TBinOp
              TyToInfer
              ShiftL
              (TConst (mkImmNativeType U64) (NativeInt 1))
              (TConst (mkImmNativeType U64) (NativeInt 2))
          ),
    -- --
    testCase "parses let stmts 1" $
      parse parseLet "" "y:   i64 = -30;" @?= Right (TLet (mkImmNativeType I64) "y" (TUnaryOp (mkImmNativeType I64) Neg (TConst (mkImmNativeType I64) (NativeInt 30)))),
    -- --
    testCase "parse integers ignores comments afterwards" $
      parse (parseExpr <* eof) "" "123 // hey you!" @?= Right (TConst (mkImmNativeType U64) (NativeInt 123)),
    -- --
    testCase "parse integers failure" $
      assertBool "" (isLeft $ parse parseUint "" "a123"),
    -- --
    -- testCase "parse program" $
    --   runProgramParser "     x : i64 = 3; y : i64 = 4; return (x + y);"
    --     @?= Right (Program [TLet (Native ImmTy I64) "x" (Const I64 (NativeInt 3)), Let (Native ImmTy I64) "y" (Const I64 (NativeInt 4)), Return (BinOp Add (Var TyToInfer "x") (Var TyToInfer "y"))]),
    -- --
    testCase "parse program with commented lines" $
      runProgramParser "x : i64 = 3; // let y : i64 = 4; \n return (x + y);"
        @?= Right
          ( TypedProgram
              [ TLet (mkImmNativeType I64) "x" (TConst (mkImmNativeType U64) (NativeInt 3)),
                TReturn TyToInfer (TBinOp TyToInfer Add (TVar TyToInfer "x") (TVar TyToInfer "y"))
              ]
          ),
    -- --
    testCase "parse whole commented program" $
      runProgramParser "// x: i64 = 4;" @?= Right (TypedProgram []),
    --
    testCase "parse invalid program fails" $
      let res = runProgramParser "x: i64 = 3 \n y: int = 4;"
       in assertBool "" (isLeft res)
  ]
