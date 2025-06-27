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
  = Neg
  | Not
  deriving (Eq)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | Lt
  | Le
  | ShiftL
  deriving (-- | ShiftR
            Eq)

type Label = T.Text

data NativeVal = NativeInt Int | NativeBool Bool
  deriving (Eq, Show)

data Expr where
  Const :: Type -> NativeVal -> Expr
  UnaryOp :: Type -> UnaryOp -> Expr -> Expr
  BinOp :: Type -> BinOp -> Expr -> Expr -> Expr
  Var :: Type -> Label -> Expr
  deriving (Eq)

data Stmt where
  Let :: Type -> Label -> Expr -> Stmt
  Print :: Type -> Expr -> Stmt
  Return :: Type -> Expr -> Stmt
  deriving (Eq)

-- A program is a sequence of statements
newtype Program = Program {progStmts :: [Stmt]}
  deriving (Show, Eq)

instance Show UnaryOp where
  show Neg = "-"
  show Not = "!"

instance Show BinOp where
  show Add = " + "
  show Sub = " - "
  show Mul = " + "
  show Div = " \\ "
  show Eq = " == "
  show Lt = " < "
  show Le = " <= "
  show ShiftL = " << "

instance Show Expr where
  show (Const _ val) = show val
  show (UnaryOp _ op expr) | isAtomicExpr expr = show op <> show expr
  show (UnaryOp _ op expr) = show op <> "(" <> show expr <> ")"
  show (BinOp _ op lh rh) | isAtomicExpr lh && isAtomicExpr rh = show lh <> show op <> show rh
  show (BinOp _ op lh rh) | isAtomicExpr lh = show lh <> show op <> "(" <> show rh <> ")"
  show (BinOp _ op lh rh) | isAtomicExpr rh = "(" <> show lh <> ")" <> show op <> show rh
  show (BinOp _ op lh rh) = "(" <> show lh <> ")" <> show op <> "(" <> show rh <> ")"
  show (Var _ label) = show label

instance Show Stmt where
  show (Let ty label expr) = "let " <> show label <> " : " <> show ty <> " = " <> show expr
  show (Print _ expr) = "print(" <> show expr <> ")"
  show (Return _ expr) = "return " <> show expr

newProgram :: [Stmt] -> Program
newProgram = Program

typeFromExpr :: Expr -> Type
typeFromExpr (Const t _) = t
typeFromExpr (UnaryOp t _ _) = t
typeFromExpr (BinOp t _ _ _) = t
typeFromExpr (Var t _) = t

isAtomicExpr :: Expr -> Bool
isAtomicExpr (Const _ _) = True
isAtomicExpr (Var _ _) = True
isAtomicExpr _ = False
