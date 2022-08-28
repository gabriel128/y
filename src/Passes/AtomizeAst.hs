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
module Passes.AtomizeAst (removeComplexStmts, runRemComplexStmts) where

import Ast.Ast
import Control.Carrier.Error.Either
import Control.Carrier.Fresh.Strict
import Control.Carrier.State.Strict
import Data.Foldable
import Data.Text (Text)
import qualified Passes.PassEffs as PassEffs
import Utils

runRemComplexStmts :: Info -> Program -> Either Text (Info, Program)
runRemComplexStmts info program = run . runError . runState info $ removeComplexStmts program

removeComplexStmts :: Program -> PassEffs.StErr sig m Program
removeComplexStmts (Program stmts) = fmap snd $
  runFresh 0 $ do
    stmts' <- foldl' reducer (pure []) stmts
    pure (Program stmts')
  where
    reducer :: PassEffs.StErrRnd sig m [Stmt] -> Stmt -> PassEffs.StErrRnd sig m [Stmt]
    reducer prevStmts stmt = do
      prevStmts' <- prevStmts
      stmts' <- removeComplexStmt stmt
      pure (prevStmts' ++ stmts')

removeComplexStmt :: Stmt -> PassEffs.StErrRnd sig m [Stmt]
removeComplexStmt (Let binding expr) = do
  (stmts, lastExpr) <- removeComplexExp expr
  pure (stmts ++ [Let binding lastExpr])
removeComplexStmt other = pure [other]

removeComplexExp :: Expr -> PassEffs.StErrRnd sig m ([Stmt], Expr)
removeComplexExp expr | isReduced expr = pure ([], expr)
removeComplexExp (UnaryOp op expr) = createSingleVar expr (UnaryOp op)
removeComplexExp (BinOp op exprL exprR)
  | isAtomic exprL = createSingleVar exprR (BinOp op exprL)
  | isAtomic exprR = createSingleVar exprL $ flip (BinOp op) exprR
  | otherwise = createDoubleVar exprL exprR (BinOp op)
removeComplexExp expr = pure ([], expr)

createSingleVar :: Expr -> (Expr -> Expr) -> PassEffs.StErrRnd sig m ([Stmt], Expr)
createSingleVar expr expConstr = do
  varName <- Utils.freshVarName fresh
  (stmts, expr') <- removeComplexExp expr
  modify (addLocal varName)
  pure (stmts ++ [Let varName expr'], expConstr (Var varName))

createDoubleVar :: Expr -> Expr -> (Expr -> Expr -> Expr) -> PassEffs.StErrRnd sig m ([Stmt], Expr)
createDoubleVar exprL exprR expConstr = do
  varNameL <- Utils.freshVarName fresh
  varNameR <- Utils.freshVarName fresh
  (stmtsL, exprL') <- removeComplexExp exprL
  (stmtsR, exprR') <- removeComplexExp exprR
  modify (addLocal varNameL)
  modify (addLocal varNameR)
  pure (stmtsL ++ [Let varNameL exprL'] ++ stmtsR ++ [Let varNameR exprR'], expConstr (Var varNameL) (Var varNameR))

isReduced :: Expr -> Bool
isReduced expr | isAtomic expr = True
isReduced (BinOp _ expr1 expr2) = isAtomic expr1 && isAtomic expr2
isReduced (UnaryOp _ expr) = isAtomic expr
isReduced _ = False

isAtomic :: Expr -> Bool
isAtomic (Const _) = True
isAtomic (Var _) = True
isAtomic _ = False
