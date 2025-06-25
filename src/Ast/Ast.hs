{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Ast.Ast where

import qualified Data.Text as T
import Types.Defs

data FuncLocalInfo = FuncLocalInfo
  deriving (Eq, Show)

data Func = Func {funcId :: T.Text, funcArgs :: [Expr], funcLocalInfo :: FuncLocalInfo}
  deriving (Eq, Show)

data UnaryOp
  = Neg | Not
  deriving (Eq, Show)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | EQ
  | LT
  | LE
  | GE
  | GT
  | ShiftL
  -- | ShiftR
  deriving (Eq, Show)

type Label = T.Text

data NativeVal = NativeInt Int | NativeBool Bool
  deriving (Eq, Show)

data Expr where
  Const :: Type -> NativeVal -> Expr
  UnaryOp :: Type -> UnaryOp -> Expr -> Expr
  BinOp :: Type -> BinOp -> Expr -> Expr -> Expr
  Var :: Type -> Label -> Expr
  deriving (Eq, Show)

data Stmt where
  Let :: Type -> Label -> Expr -> Stmt
  Print :: Type -> Expr -> Stmt
  Return :: Type -> Expr -> Stmt
  deriving (Eq, Show)

-- A program is a sequence of statements
newtype Program = Program {progStmts :: [Stmt]}
  deriving (Show, Eq)

newProgram :: [Stmt] -> Program
newProgram = Program

typeFromExpr :: Expr -> Type
typeFromExpr (Const t _) = t
typeFromExpr (UnaryOp t _ _) = t
typeFromExpr (BinOp t _ _ _) = t
typeFromExpr (Var t _) = t
