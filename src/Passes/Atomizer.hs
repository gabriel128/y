-- Atomizer
--
-- removes complex expressions in statments
-- and transforms them in atomic variables
--
-- e.g.
-- let x = (1 + (2 + (3 + 4)))
-- =>
-- let y = (2 + (3 + 4)); let x = 1 + y
-- =>
-- let z = 3 + 4; let y = 2 + z; let x = 1 + y
module Passes.Atomizer where

import Ast.Ast
import Context (Context, addLocal)
import Control.Carrier.Error.Either
import Control.Carrier.Fresh.Strict
import Control.Carrier.State.Strict
import Data.Foldable
import Data.Text (Text)
import EffUtils (StateErrorEff, StateErrorEffM, StateErrorRndEff, StateErrorRndEffM)
import Types.Defs (Type (TyToInfer))
import Utils

-- Extracts the Context and Program from the effects
runRemComplexStmts :: Context -> Program -> Either Text (Context, Program)
runRemComplexStmts info program = run . runError . runState info $ removeComplexStmts program

--  === Public Api ===

removeComplexStmts :: Program -> StateErrorEffM Context Text m Program
removeComplexStmts (Program stmts) = fmap snd $
  runFresh 0 $ do
    stmts' <- foldl' reducer (pure []) stmts
    addBindsToContext stmts'
    pure (Program stmts')
  where
    reducer :: StateErrorRndEffM Context Text m [Stmt] -> Stmt -> StateErrorRndEffM Context Text m [Stmt]
    reducer prevStmts stmt = do
      prevStmts' <- prevStmts
      stmts' <- removeComplexStmt stmt
      pure (prevStmts' ++ stmts')

-- === Private ====

addBindsToContext :: [Stmt] -> StateErrorEff Context Text ()
addBindsToContext = mapM_ mapper
  where
    mapper :: Stmt -> StateErrorEff Context Text ()
    mapper (Let _ binding _) = modify @Context (Context.addLocal binding)
    mapper _ = pure ()

--  Transform a complex statment (i.e. statements that are not atomic) into sequential let bindings
removeComplexStmt :: Stmt -> StateErrorRndEff Context Text [Stmt]
removeComplexStmt stmt =
  case stmt of
    stmt'@(Return expr) | isAtomic expr -> pure [stmt']
    stmt'@(Print expr) | isAtomic expr -> pure [stmt']
    stmt'@(Let _ _ expr) | isAtomic expr -> pure [stmt']
    Return expr -> do
      (letStmts, lastExpr) <- letsFromComplexExp expr
      varName <- Utils.freshVarName fresh
      pure (letStmts ++ [Let TyToInfer varName lastExpr, Return (Var TyToInfer varName)])
    Print expr -> do
      (letStmts, lastExpr) <- letsFromComplexExp expr
      varName <- Utils.freshVarName fresh
      pure (letStmts ++ [Let TyToInfer varName lastExpr, Print (Var TyToInfer varName)])
    Let ty binding expr -> do
      (stmts, lastExpr) <- letsFromComplexExp expr
      pure (stmts ++ [Let TyToInfer binding lastExpr])

-- Creates let statements from complex expressions
letsFromComplexExp :: Expr -> StateErrorRndEff Context Text ([Stmt], Expr)
letsFromComplexExp expr' =
  case expr' of
    expr | isReduced expr -> pure ([], expr)
    UnaryOp op expr -> createLetBinding expr (UnaryOp op)
    BinOp op exprL exprR | isAtomic exprL -> createLetBinding exprR (BinOp op exprL)
    BinOp op exprL exprR | isAtomic exprR -> createLetBinding exprL $ flip (BinOp op) exprR
    BinOp op exprL exprR -> createDoubleLetBinding exprL exprR (BinOp op)
    expr -> pure ([], expr)

-- | Creates a single let statement, it will have the shape of tmp_x
--   where x is an incremental number
createLetBinding :: Expr -> (Expr -> Expr) -> StateErrorRndEff Context Text ([Stmt], Expr)
createLetBinding expr expConstr = do
  varName <- Utils.freshVarName fresh
  (stmts, expr') <- letsFromComplexExp expr
  pure (stmts ++ [Let TyToInfer varName expr'], expConstr (Var TyToInfer varName))

-- | Utility function to create two let bindings at one from one
createDoubleLetBinding :: Expr -> Expr -> (Expr -> Expr -> Expr) -> StateErrorRndEff Context Text ([Stmt], Expr)
createDoubleLetBinding exprL exprR expConstr = do
  varNameL <- Utils.freshVarName fresh
  varNameR <- Utils.freshVarName fresh
  (stmtsL, exprL') <- letsFromComplexExp exprL
  (stmtsR, exprR') <- letsFromComplexExp exprR
  pure (stmtsL ++ [Let TyToInfer varNameL exprL'] ++ stmtsR ++ [Let TyToInfer varNameR exprR'], expConstr (Var TyToInfer varNameL) (Var TyToInfer varNameR))

-- If it's reduced it means that it can't be reduced further
isReduced :: Expr -> Bool
isReduced expr | isAtomic expr = True
isReduced (BinOp _ expr1 expr2) = isAtomic expr1 && isAtomic expr2
isReduced (UnaryOp _ expr) = isAtomic expr
isReduced _ = False

isAtomic :: Expr -> Bool
isAtomic (Const _ _) = True
isAtomic (Var _ _) = True
isAtomic _ = False
