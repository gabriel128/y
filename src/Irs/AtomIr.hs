{-# LANGUAGE GADTs #-}

module Irs.AtomIr where

import Ast.Ast
import Data.Foldable (Foldable (foldr'))
import Types.Defs

{- | Atomic and Typed IR _a la_ TAC (three address code)),
  i.e. there is no nesting and it's not gluten free
-}
data AExpr t where
    ALit :: Literal Type -> AExpr t
    AUnaryOp :: Type -> UnaryOp -> Literal Type -> AExpr t
    ABinOp :: Type -> BinOp -> Literal Type -> Literal Type -> AExpr t
    deriving (Eq, Show)

type AtomicStmt = Stmt AExpr Type

isEqALit :: Literal t -> Literal t' -> Bool
isEqALit (LNum _ lit) (LNum _ lit') = lit == lit'
isEqALit (LVar _ label) (LVar _ label') = label == label'
isEqALit (LBool _ lit) (LBool _ lit') = lit == lit'

isEquivalentExpr :: Expr t -> AExpr t' -> Bool
isEquivalentExpr (Lit lit) (ALit lit') = isEqALit lit lit'
isEquivalentExpr (UnaryOp _ op (Lit lit)) (AUnaryOp _ op' lit') = isEqALit lit lit'

areEquivalentStmts :: [Stmt Expr Type] -> [Stmt AExpr Type] -> Bool
areEquivalentStmts [] [] = True
areEquivalentStmts stmts [] = False
areEquivalentStmts [] stmts = False
areEquivalentStmts (x : exprs) (y : aExprs) = undefined

-- instance PrettyPrint AExpr
