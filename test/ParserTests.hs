{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module ParserTests (test_parser) where

import Ast.Ast
import qualified Ast.Ast as Ast
import qualified Ast.Ast as BinOp
import Data.Either (isLeft)
import Parser.Parser
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))
import Text.Megaparsec
import Types.Defs
import Utils (liftEither)

-- import Test.Tasty.SmallCheck as SC

test_parser :: TestTree
test_parser = testGroup "Parser Tests" unitTests

expr :: Expr
expr =
    let negEight = UnaryOp TyToInfer Neg (Const I64 "8")
        ast1_1 = BinOp TyToInfer Add (Const I64 "42") negEight
     in ast1_1

unitTests :: [TestTree]
unitTests =
    [ testCase "parse integers success" $
        parse parseExpr "" "123" @?= Right (Const I64 "123")
    , --
      testCase "parses negative integer success" $
        parse parseExpr "" "-123" @?= Right (UnaryOp (MkNativeType I64) Neg (Const I64 "123"))
    , --
      testCase "parses let stmts with const" $
        parse parseLet "" "x : u64 = 3;" @?= Right (Let (MkNativeType U64) "x" (Const I64 "3"))
    , --
      -- testCase "parses let stmts with mut" $
      --   parse parseLet "" "x mut u64 = 3;" @?= Right (Let (mkMutNativeType U64) "x" (Const (MkNativeType I64) ( 3)))
      -- , --
      testCase "parses let stmts with var" $
        parse parseLet "" "x:i64 = y;" @?= Right (Let (MkNativeType I64) "x" (Var TyToInfer "y"))
    , --
      testCase "parses return stmt" $
        parse parseReturn "" "return y;" @?= Right (Return TyToInfer (Var TyToInfer "y"))
    , --
      testCase "parses let stmt with sum" $
        parse parseLet "" "x: u64 = 1 + y;"
            @?= Right
                ( Let
                    (MkNativeType U64)
                    "x"
                    (BinOp TyToInfer Add (Const I64 "1") (Var TyToInfer "y"))
                )
    , --
      testCase "pepe parses let stmt with inference" $ do
        parsedStmt <- liftEither $ parse parseLetToInfer "" "x = 1 + 3;"
        assertEqual "" parsedStmt (Let TyToInfer "x" (BinOp TyToInfer Add (Const I64 "1") (Const I64 "3")))
    , ---
      testCase "parses sum exprs" $
        parse (parseExpr <* eof) "" "(1 + 3) - 2"
            @?= Right
                ( BinOp
                    TyToInfer
                    Sub
                    (BinOp TyToInfer Add (Const I64 "1") (Const I64 "3"))
                    (Const I64 "2")
                )
    , testCase "parses less than exprs" $
        parse (parseExpr <* eof) "" "1 < 2"
            @?= Right
                ( Ast.BinOp
                    (MkNativeType TyBool)
                    BinOp.Lt
                    (Const I64 "1")
                    (Const I64 "2")
                )
    , testCase "parses grater than exprs" $
        parse (parseExpr <* eof) "" "1 > 2"
            @?= Right
                ( Ast.BinOp
                    (MkNativeType TyBool)
                    BinOp.Lt
                    (Const I64 "2")
                    (Const I64 "1")
                )
    , testCase "parses less than equal exprs" $
        parse (parseExpr <* eof) "" "1 <= 2"
            @?= Right
                ( Ast.BinOp
                    (MkNativeType TyBool)
                    BinOp.Le
                    (Const I64 "1")
                    (Const I64 "2")
                )
    , testCase "parses grater than equal exprs" $
        parse (parseExpr <* eof) "" "1 >= 2"
            @?= Right
                ( Ast.BinOp
                    (MkNativeType TyBool)
                    BinOp.Le
                    (Const I64 "2")
                    (Const I64 "1")
                )
    , testCase "parses equal exprs" $
        parse (parseExpr <* eof) "" "1 == 2"
            @?= Right
                ( Ast.BinOp
                    (MkNativeType TyBool)
                    BinOp.Eq
                    (Const I64 "1")
                    (Const I64 "2")
                )
    , testCase "parses mult-sums stmts" $
        parse (parseExpr <* eof) "" "1 * 3 + 2"
            @?= Right
                ( BinOp
                    TyToInfer
                    Add
                    (BinOp TyToInfer Mul (Const I64 "1") (Const I64 "3"))
                    (Const I64 "2")
                )
    , -- --
      testCase "parses shift left with vars" $
        parse (parseExpr <* eof) "" "y << 2" @?= Right (BinOp TyToInfer ShiftL (Var TyToInfer "y") (Const I64 "2"))
    , -- --
      testCase "parses shift left with num" $
        parse (parseExpr <* eof) "" "1 << 2"
            @?= Right
                ( BinOp
                    TyToInfer
                    ShiftL
                    (Const I64 "1")
                    (Const I64 "2")
                )
    , -- --
      testCase "parses let stmts 1" $
        parse parseLet "" "y:   i64 = -30;" @?= Right (Let (MkNativeType I64) "y" (UnaryOp (MkNativeType I64) Neg (Const I64 "30")))
    , -- --
      testCase "parse integers ignores comments afterwards" $
        parse (parseExpr <* eof) "" "123 // hey you!" @?= Right (Const I64 "123")
    , -- --
      testCase "parse integers failure" $
        assertBool "" (isLeft $ parse parseUint "" "a123")
    , -- --
      -- testCase "parse program" $
      --   runProgramParser "     x : i64 = 3; y : i64 = 4; return (x + y);"
      --     @?= Right (Program [Let (Native ImmTy I64) "x" (Const I64 ( 3)), Let (Native ImmTy I64) "y" (Const I64 ( 4)), Return (BinOp Add (Var TyToInfer "x") (Var TyToInfer "y"))]),
      -- --
      testCase "parse program with commented lines" $
        runProgramParser "x : i64 = 3; // let y : i64 = 4; \n return (x + y);"
            @?= Right
                ( Program
                    [ Let (MkNativeType I64) "x" (Const I64 "3")
                    , Return TyToInfer (BinOp TyToInfer Add (Var TyToInfer "x") (Var TyToInfer "y"))
                    ]
                )
    , -- --
      testCase "parse whole commented program" $
        runProgramParser "// x: i64 = 4;" @?= Right (Program [])
    , --
      testCase "parse invalid program fails" $
        let res = runProgramParser "x: i64 = 3 \n y: int = 4;"
         in assertBool "" (isLeft res)
    ]
