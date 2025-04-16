{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Ast.Ast where

import qualified Data.Text as T

data FuncLocalInfo = FuncLocalInfo
  deriving (Eq, Show)

data Func = Func {funcId :: T.Text, funcArgs :: [Expr], funcLocalInfo :: FuncLocalInfo}
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

data NativeVal = NativeInt Int | NativeBool Bool
  deriving (Eq, Show)

data Expr where
  Const :: NativeVal -> Expr
  UnaryOp :: UnaryOp -> Expr -> Expr
  BinOp :: BinOp -> Expr -> Expr -> Expr
  Var :: Label -> Expr
  deriving (Eq, Show)

data Stmt where
  Let :: Label -> Expr -> Stmt
  Print :: Expr -> Stmt
  Return :: Expr -> Stmt
  deriving (Eq, Show)

-- A program is a sequence of statements
newtype Program = Program {progStmts :: [Stmt]}
  deriving (Show, Eq)

newProgram :: [Stmt] -> Program
newProgram = Program
