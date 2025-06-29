{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Ast.Ast where

import Data.Text
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Types.Defs
import Utils

data FuncLocalInfo = FuncLocalInfo
    deriving (Eq, Show)

data Func = Func {funcId :: T.Text, funcArgs :: [Expr], funcLocalInfo :: FuncLocalInfo}
    deriving (Eq, Show)

data UnaryOp
    = Neg
    | Not
    deriving (Eq, Show)

data BinOp
    = Add
    | Sub
    | Mul
    | Div
    | Eq
    | Lt
    | Le
    | -- | ShiftR
      ShiftL
    deriving (Eq, Show)

type Label = T.Text

data Expr where
    Const :: NativeType -> Text -> Expr
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
typeFromExpr (Const t _) = MkNativeType t
typeFromExpr (UnaryOp t _ _) = t
typeFromExpr (BinOp t _ _ _) = t
typeFromExpr (Var t _) = t

isAtomicExpr :: Expr -> Bool
isAtomicExpr (Const _ _) = True
isAtomicExpr (Var _ _) = True
isAtomicExpr _ = False
