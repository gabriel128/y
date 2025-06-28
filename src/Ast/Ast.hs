{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Ast.Ast where

import Data.Text
import qualified Data.Text as T
import Types.Defs
import Utils
import Utils (PrettyPrint (prettyPrint))

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

instance PrettyPrint Label where
    prettyPrint = tshow

instance PrettyPrint UnaryOp where
    prettyPrint Neg = "-"
    prettyPrint Not = "!"

instance PrettyPrint BinOp where
    prettyPrint Add = " + "
    prettyPrint Sub = " - "
    prettyPrint Mul = " + "
    prettyPrint Div = " \\ "
    prettyPrint Eq = " == "
    prettyPrint Lt = " < "
    prettyPrint Le = " <= "
    prettyPrint ShiftL = " << "

instance PrettyPrint Expr where
    prettyPrint (Const _ val) = prettyPrint val
    prettyPrint (UnaryOp _ op expr) | isAtomicExpr expr = prettyPrint op <> prettyPrint expr
    prettyPrint (UnaryOp _ op expr) = prettyPrint op <> "(" <> prettyPrint expr <> ")"
    prettyPrint (BinOp _ op lh rh) | isAtomicExpr lh && isAtomicExpr rh = prettyPrint lh <> prettyPrint op <> prettyPrint rh
    prettyPrint (BinOp _ op lh rh) | isAtomicExpr lh = prettyPrint lh <> prettyPrint op <> "(" <> prettyPrint rh <> ")"
    prettyPrint (BinOp _ op lh rh) | isAtomicExpr rh = "(" <> prettyPrint lh <> ")" <> prettyPrint op <> prettyPrint rh
    prettyPrint (BinOp _ op lh rh) = "(" <> prettyPrint lh <> ")" <> prettyPrint op <> "(" <> prettyPrint rh <> ")"
    prettyPrint (Var _ label) = prettyPrint label

instance PrettyPrint Stmt where
    prettyPrint (Let ty label expr) = "let " <> prettyPrint label <> " : " <> prettyPrint ty <> " = " <> prettyPrint expr
    prettyPrint (Print _ expr) = "print(" <> prettyPrint expr <> ")"
    prettyPrint (Return _ expr) = "return " <> prettyPrint expr

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
