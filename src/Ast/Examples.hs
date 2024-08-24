module Ast.Examples where

import Ast.Ast
import Types.Defs

stmt0 :: Stmt
stmt0 = Let (Native TyInt) "x" (Const TyInt 10)

stmt1 :: Stmt
stmt1 = Let (Native TyInt) "y" (Var (Native TyInt) "x")

stmt2 :: Stmt
stmt2 = Return (BinOp Add (Var (Native TyInt) "y") (UnaryOp Neg (Const TyInt 8)))

stmt3 :: Stmt
stmt3 = Return (BinOp Add (Var (Native TyInt) "z") (UnaryOp Neg (Const TyInt 7)))

testProg :: Program
testProg = Program [stmt0, stmt1, stmt2, stmt3]
