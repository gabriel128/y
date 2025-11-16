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

expr :: Expr (Maybe Type)
expr =
    let negEight = UnaryOp (Just I64) Neg (Lit $ LNum I64 8)
        ast1_1 = BinOp (Just I64) Add (Lit $ LNum I64 42) negEight
     in ast1_1

unitTests :: [TestTree]
unitTests =
    [ testCase "parse integers success" $
        parse parseExpr "" "123" @?= Right (Lit $ LNum I64 123)
    , --
      testCase "parses negative integer success" $
        parse parseExpr "" "-123" @?= Right (UnaryOp (Just I64) Neg (Lit $ LNum I64 123))
    , --
      testCase "parses true bool" $
        parse parseExpr "" "true" @?= Right (Lit $ LBool TyBool True)
    , testCase "parses false bool" $
        parse parseExpr "" "false" @?= Right (Lit $ LBool TyBool False)
    , --
      testCase "parses let stmts with const" $
        parse parseLet "" "x : u64 = 3;" @?= Right (Let (Just U64) "x" (Lit $ LNum I64 3))
    , --
      -- testCase "parses let stmts with mut" $
      --   parse parseLet "" "x mut u64 = 3;" @?= Right (Let (mkMutNativeType U64) "x" (Lit $ LNum (Just I64) ( 3)))
      -- , --
      testCase "parses let stmts with var" $
        parse parseLet "" "x:i64 = y;" @?= Right (Let (Just I64) "x" (Lit $ LVar Nothing "y"))
    , --
      testCase "parses let stmts with bools" $
        parse parseLet "" "x:bool = true;" @?= Right (Let (Just TyBool) "x" (Lit $ LBool TyBool True))
    , --
      testCase "parses return stmt" $
        parse parseReturn "" "return y;" @?= Right (Return Nothing (Lit $ LVar Nothing "y"))
    , --
      testCase "parses let stmt with sum" $
        parse parseLet "" "x: u64 = 1 + y;"
            @?= Right
                ( Let
                    (Just U64)
                    "x"
                    (BinOp Nothing Add (Lit $ LNum I64 1) (Lit $ LVar Nothing "y"))
                )
    , --
      testCase "pepe parses let stmt with inference" $ do
        parsedStmt <- liftEither $ parse parseLetToInfer "" "x = 1 + 3;"
        assertEqual "" parsedStmt (Let Nothing "x" (BinOp Nothing Add (Lit $ LNum I64 1) (Lit $ LNum I64 3)))
    , ---
      testCase "parses sum exprs" $
        parse (parseExpr <* eof) "" "(1 + 3) - 2"
            @?= Right
                ( BinOp
                    Nothing
                    Sub
                    (BinOp Nothing Add (Lit $ LNum I64 1) (Lit $ LNum I64 3))
                    (Lit $ LNum I64 2)
                )
    , testCase "parses less than exprs" $
        parse (parseExpr <* eof) "" "1 < 2"
            @?= Right
                ( Ast.BinOp
                    (Just TyBool)
                    BinOp.Lt
                    (Lit $ LNum I64 1)
                    (Lit $ LNum I64 2)
                )
    , testCase "parses grater than exprs" $
        parse (parseExpr <* eof) "" "1 > 2"
            @?= Right
                ( Ast.BinOp
                    (Just TyBool)
                    BinOp.Lt
                    (Lit $ LNum I64 2)
                    (Lit $ LNum I64 1)
                )
    , testCase "parses less than equal exprs" $
        parse (parseExpr <* eof) "" "1 <= 2"
            @?= Right
                ( Ast.BinOp
                    (Just TyBool)
                    BinOp.Le
                    (Lit $ LNum I64 1)
                    (Lit $ LNum I64 2)
                )
    , testCase "parses grater than equal exprs" $
        parse (parseExpr <* eof) "" "1 >= 2"
            @?= Right
                ( Ast.BinOp
                    (Just TyBool)
                    BinOp.Le
                    (Lit $ LNum I64 2)
                    (Lit $ LNum I64 1)
                )
    , testCase "parses equal exprs" $
        parse (parseExpr <* eof) "" "1 == 2"
            @?= Right
                ( Ast.BinOp
                    (Just TyBool)
                    BinOp.Eq
                    (Lit $ LNum I64 1)
                    (Lit $ LNum I64 2)
                )
    , testCase "parses not equal exprs" $
        parse (parseExpr <* eof) "" "1 != 2"
            @?= Right
                ( Ast.BinOp
                    (Just TyBool)
                    BinOp.Neq
                    (Lit $ LNum I64 1)
                    (Lit $ LNum I64 2)
                )
    , testCase "parses mult-sums stmts" $
        parse (parseExpr <* eof) "" "1 * 3 + 2"
            @?= Right
                ( BinOp
                    Nothing
                    Add
                    (BinOp Nothing Mul (Lit $ LNum I64 1) (Lit $ LNum I64 3))
                    (Lit $ LNum I64 2)
                )
    , -- --
      testCase "parses shift left with vars" $
        parse (parseExpr <* eof) "" "y << 2" @?= Right (BinOp Nothing ShiftL (Lit $ LVar Nothing "y") (Lit $ LNum I64 2))
    , -- --
      testCase "parses shift left with num" $
        parse (parseExpr <* eof) "" "1 << 2"
            @?= Right
                ( BinOp
                    Nothing
                    ShiftL
                    (Lit $ LNum I64 1)
                    (Lit $ LNum I64 2)
                )
    , -- --
      testCase "parses let stmts 1" $
        parse parseLet "" "y:   i64 = -30;" @?= Right (Let (Just I64) "y" (UnaryOp (Just I64) Neg (Lit $ LNum I64 30)))
    , -- --
      testCase "parse integers ignores comments afterwards" $
        parse (parseExpr <* eof) "" "123 // hey you!" @?= Right (Lit $ LNum I64 123)
    , -- --
      testCase "parse integers failure" $
        assertBool "" (isLeft $ parse parseUint "" "a123")
    , -- --
      -- testCase "parse program" $
      --   runProgramParser "     x : i64 = 3; y : i64 = 4; return (x + y);"
      --     @?= Right (Program [Let (Native ImmTy I64) "x" (Lit $ LNum I64 ( 3)), Let (Native ImmTy I64) "y" (Lit $ LNum I64 ( 4)), Return (BinOp Add (Lit $ LVar Nothing "x") (Lit $ LVar Nothing "y"))]),
      -- --
      testCase "parse program with commented lines" $
        runProgramParser "x : i64 = 3; // let y : i64 = 4; \n return (x + y);"
            @?= Right
                ( Program
                    [ Let (Just I64) "x" (Lit $ LNum I64 3)
                    , Return Nothing (BinOp Nothing Add (Lit $ LVar Nothing "x") (Lit $ LVar Nothing "y"))
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
