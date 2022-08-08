module Ast.Examples where

import Ast.Ast

stmt0 :: Stmt
stmt0 = Let "x" (Const 10)

stmt1 :: Stmt
stmt1 = Let "y" (Var "x")

stmt2 :: Stmt
stmt2 = Return (BinOp Add (Var "y") (UnaryOp Neg (Const 8)))

stmt3 :: Stmt
stmt3 = Return (BinOp Add (Var "z") (UnaryOp Neg (Const 7)))

testProg :: Program
testProg = Program [stmt0, stmt1, stmt2, stmt3]
