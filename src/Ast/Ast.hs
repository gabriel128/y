module Ast.Ast where

import qualified Data.Text as T
import Types.Defs

data Func = Func {funcId :: T.Text, funcArgs :: [Expr]}
  deriving (Eq, Show)

data UnaryOp
  = Neg
  deriving (Eq, Show)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | ShiftL
  deriving (Eq, Show)

type Label = T.Text

data Expr
  = Const NativeType Int
  | UnaryOp UnaryOp Expr
  | BinOp BinOp Expr Expr
  | Var Type Label
  deriving (Eq, Show)

data LetModifier = Imm | Mut
  deriving (Eq, Show)

data Stmt
  = Let Type Label Expr
  | Print Expr
  | Return Expr
  deriving (Eq, Show)

-- A program is a sequence of statements
newtype Program = Program {progStmts :: [Stmt]} deriving (Show, Eq)

newProgram :: [Stmt] -> Program
newProgram = Program
