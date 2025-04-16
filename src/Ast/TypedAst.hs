{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Ast.TypedAst where

import qualified Ast.Ast as Ast
import Types.Defs

data TypedExpr where
  TConst :: Type -> Ast.NativeVal -> TypedExpr
  TUnaryOp :: Type -> Ast.UnaryOp -> TypedExpr -> TypedExpr
  TBinOp :: Type -> Ast.BinOp -> TypedExpr -> TypedExpr -> TypedExpr
  TVar :: Type -> Ast.Label -> TypedExpr
  deriving (Eq, Show)

data TypedStmt where
  TLet :: Type -> Ast.Label -> TypedExpr -> TypedStmt
  TPrint :: Type -> TypedExpr -> TypedStmt
  TReturn :: Type -> TypedExpr -> TypedStmt
  deriving (Eq, Show)

-- A program is a sequence of statements
newtype TypedProgram = TypedProgram {typedProgStmts :: [TypedStmt]}
  deriving (Eq, Show)

class FromTo a b where
  from :: a -> b

instance FromTo TypedStmt Ast.Stmt where
  from (TLet _ l texpr) = Ast.Let l (from texpr)
  from (TPrint _ texpr) = Ast.Print (from texpr)
  from (TReturn _ texpr) = Ast.Return (from texpr)

instance FromTo TypedExpr Ast.Expr where
  from (TConst _ nval) = Ast.Const nval
  from (TVar _ l) = Ast.Var l
  from (TUnaryOp _ op texpr) = Ast.UnaryOp op (from texpr)
  from (TBinOp _ op texprl texprr) = Ast.BinOp op (from texprl) (from texprr)

newTypedProgram :: [TypedStmt] -> TypedProgram
newTypedProgram = TypedProgram

typeFromStmt :: TypedStmt -> Type
typeFromStmt (TLet t _ _) = t
typeFromStmt _ = TyNative TypeMeta {mut = False} Unit
