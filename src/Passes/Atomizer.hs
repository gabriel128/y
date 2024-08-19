{-# LANGUAGE RankNTypes #-}

-- |
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
import Control.Carrier.Error.Either
import Control.Carrier.Fresh.Strict
import Control.Carrier.State.Strict
import Data.Foldable
import Data.Text (Text)
import qualified Passes.PassEffs as PassEffs
import Utils
import Context (Context, addLocal)

-- Setup Doc test

-- $setup
-- >>> import Ast.Ast
-- >>> import Control.Carrier.Error.Either
-- >>> import Control.Carrier.Fresh.Strict
-- >>> import Control.Carrier.State.Strict
-- >>> import qualified Passes.PassEffs as PassEffs
-- >>> import Passes.PassEffs (runStErr)
-- >>> import Data.Either (fromRight)
-- >>> import Context (Context, defaultContext)

-- Extracts the Context and Program from the effects
runRemComplexStmts :: Context -> Program -> Either Text (Context, Program)
runRemComplexStmts info program = run . runError . runState info $ removeComplexStmts program

--  === Public Api ===

removeComplexStmts :: Program -> PassEffs.StErr sig m Program
removeComplexStmts (Program stmts) = fmap snd $
  runFresh 0 $ do
    stmts' <- foldl' reducer (pure []) stmts
    addBindsToContext stmts'
    pure (Program stmts')
  where
    reducer :: PassEffs.StErrRnd sig m [Stmt] -> Stmt -> PassEffs.StErrRnd sig m [Stmt]
    reducer prevStmts stmt = do
      prevStmts' <- prevStmts
      stmts' <- removeComplexStmt stmt
      pure (prevStmts' ++ stmts')

-- === Private ====

addBindsToContext :: [Stmt] -> PassEffs.StErr sig m ()
addBindsToContext [] = pure ()
addBindsToContext (x : stmts) =
    case x of
      Let binding _ -> do
        modify (Context.addLocal binding)
        addBindsToContext stmts
        pure ()
      _stmt -> do
        addBindsToContext stmts
        pure ()


--  Transform a complex statment (i.e. statements that are not atomic) into sequential let bindings
removeComplexStmt :: Stmt -> PassEffs.StErrRnd sig m [Stmt]
removeComplexStmt stmt =
  case stmt of
    stmt'@(Return expr) | isAtomic expr -> pure [stmt']
    stmt'@(Print expr) | isAtomic expr -> pure [stmt']
    stmt'@(Let _ expr) | isAtomic expr -> pure [stmt']

    Return expr -> do
      (letStmts, lastExpr) <- letsFromComplexExp expr
      varName <- Utils.freshVarName fresh
      pure (letStmts ++ [Let varName lastExpr, Return (Var varName)])
    Print expr -> do
      (letStmts, lastExpr) <- letsFromComplexExp expr
      varName <- Utils.freshVarName fresh
      pure (letStmts ++ [Let varName lastExpr, Print (Var varName)])
    Let binding expr -> do
      (stmts, lastExpr) <- letsFromComplexExp expr
      pure (stmts ++ [Let binding lastExpr])

-- Creates let statements from complex expressions
letsFromComplexExp :: Expr -> PassEffs.StErrRnd sig m ([Stmt], Expr)
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

createLetBinding :: Expr -> (Expr -> Expr) -> PassEffs.StErrRnd sig m ([Stmt], Expr)
createLetBinding expr expConstr = do
  varName <- Utils.freshVarName fresh
  (stmts, expr') <- letsFromComplexExp expr
  pure (stmts ++ [Let varName expr'], expConstr (Var varName))

-- | Utility function to create two let bindings at one from one
createDoubleLetBinding :: Expr -> Expr -> (Expr -> Expr -> Expr) -> PassEffs.StErrRnd sig m ([Stmt], Expr)
createDoubleLetBinding exprL exprR expConstr = do
  varNameL <- Utils.freshVarName fresh
  varNameR <- Utils.freshVarName fresh
  (stmtsL, exprL') <- letsFromComplexExp exprL
  (stmtsR, exprR') <- letsFromComplexExp exprR
  pure (stmtsL ++ [Let varNameL exprL'] ++ stmtsR ++ [Let varNameR exprR'], expConstr (Var varNameL) (Var varNameR))

-- If it's reduced it means that it can't be reduced further
isReduced :: Expr -> Bool
isReduced expr | isAtomic expr = True
isReduced (BinOp _ expr1 expr2) = isAtomic expr1 && isAtomic expr2
isReduced (UnaryOp _ expr) = isAtomic expr
isReduced _ = False

isAtomic :: Expr -> Bool
isAtomic (Const _) = True
isAtomic (Var _) = True
isAtomic _ = False
