module Ast.Examples where

import Ast.Ast
import Types.Defs

stmt0 :: Stmt
stmt0 = Let (Native ImmTy I64) "x" (Const I64 (NativeInt 10))

stmt1 :: Stmt
stmt1 = Let (Native ImmTy I64) "y" (Var (Native ImmTy I64) "x")

stmt2 :: Stmt
stmt2 = Return (BinOp Add (Var (Native ImmTy I64) "y") (UnaryOp Neg (Const I64 (NativeInt 8))))

stmt3 :: Stmt
stmt3 = Return (BinOp Add (Var (Native ImmTy I64) "z") (UnaryOp Neg (Const I64 (NativeInt 7))))

testProg :: Program
testProg = Program [stmt0, stmt1, stmt2, stmt3]
