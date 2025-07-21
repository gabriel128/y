{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Ast.Ast where

import qualified Data.Text as T
import Types.Defs

data FuncLocalInfo = FuncLocalInfo
    deriving (Eq, Show)

-- data Func a = Func {funcId :: T.Text, funcArgs :: [Expr a], funcLocalInfo :: FuncLocalInfo}
--     deriving (Eq, Show)

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

data Expr t where
    Lit :: Literal t -> Expr t
    UnaryOp :: t -> UnaryOp -> Expr t -> Expr t
    BinOp :: t -> BinOp -> Expr t -> Expr t -> Expr t
    deriving (Eq, Show)

{- | Stmt is parameterized with 2 arguments. `t` is type related, e.g. `Maybe Type` or `Type`
     and also takes an `expr` which takes a t to build an expression kind
-}
data Stmt expr t where
    Let :: t -> Label -> expr t -> Stmt expr t
    Print :: Type -> expr t -> Stmt expr t
    Return :: t -> expr t -> Stmt expr t
    deriving (Eq, Show)

data Literal t where
    LNum :: Type -> Int -> Literal t
    LBool :: Type -> Bool -> Literal t
    LVar :: t -> Label -> Literal t
    deriving (Eq, Show)

-- A program is a sequence of statements
newtype Program a t = Program {progStmts :: [Stmt a t]}
    deriving (Show, Eq)

type ParsedStmt = Stmt Expr (Maybe Type)

newProgram :: [Stmt a t] -> Program a t
newProgram = Program

typeFromExpr :: Expr Type -> Type
typeFromExpr (Lit (LNum t _)) = t
typeFromExpr (Lit (LBool t _)) = t
typeFromExpr (UnaryOp t _ _) = t
typeFromExpr (BinOp t _ _ _) = t
typeFromExpr (Lit (LVar t _)) = t

isAtomicExpr :: Expr t -> Bool
isAtomicExpr (Lit _) = True
isAtomicExpr _ = False
