
-- TypedAtomizer
--
-- removes complex expressions in statments
-- and transforms them in atomic variables
--
-- Works on a TypedProgram directly
--
-- e.g.
-- let x = (1 + (2 + (3 + 4)))
-- =>
-- let y = (2 + (3 + 4)); let x = 1 + y
-- =>
-- let z = 3 + 4; let y = 2 + z; let x = 1 + y
module Passes.TypedAtomizer where

import Ast.Ast
import Context (Context, addLocal)
import Control.Carrier.Error.Either
import Control.Carrier.Fresh.Strict
import Control.Carrier.State.Strict
import Data.Foldable
import Data.Text (Text)
import EffUtils (StateErrorEff, StateErrorEffM, StateErrorRndEff, StateErrorRndEffM)
import Utils
import Ast.TypedAst (TypedProgram (..), TypedStmt (..), TypedExpr (..), typeFromTExpr)
import Types.Defs (Type)

-- Extracts the Context and Program from the effects
runRemComplexStmts :: Context -> TypedProgram -> Either Text (Context, TypedProgram)
runRemComplexStmts info program = run . runError . runState info $ removeComplexStmts program

--  === Public Api ===

removeComplexStmts :: TypedProgram -> StateErrorEffM Context Text m TypedProgram
removeComplexStmts (TypedProgram stmts) = fmap snd $
  runFresh 0 $ do
    stmts' <- foldl' reducer (pure []) stmts
    addBindsToContext stmts'
    pure (TypedProgram stmts')
  where
    reducer :: StateErrorRndEffM Context Text m [TypedStmt] -> TypedStmt -> StateErrorRndEffM Context Text m [TypedStmt]
    reducer prevStmts stmt = do
      prevStmts' <- prevStmts
      stmts' <- removeComplexStmt stmt
      pure (prevStmts' ++ stmts')

-- === Private ====

addBindsToContext :: [TypedStmt] -> StateErrorEff Context Text ()
addBindsToContext = mapM_ mapper
  where
    mapper :: TypedStmt -> StateErrorEff Context Text ()
    mapper (TLet _ binding _) = modify @Context (Context.addLocal binding)
    mapper _ = pure ()

--  Transform a complex statment (i.e. statements that are not atomic) into sequential let bindings
removeComplexStmt :: TypedStmt -> StateErrorRndEff Context Text [TypedStmt]
removeComplexStmt stmt =
  case stmt of
    stmt'@(TReturn _ expr) | isAtomic expr -> pure [stmt']
    stmt'@(TPrint _ expr) | isAtomic expr -> pure [stmt']
    stmt'@(TLet _ _ expr) | isAtomic expr -> pure [stmt']
    TReturn ty expr -> do
      (letStmts, lastExpr) <- letsFromComplexExp expr
      varName <- Utils.freshVarName fresh
      pure (letStmts ++ [TLet ty varName lastExpr, TReturn ty (TVar ty varName)])
    TPrint ty expr -> do
      (letStmts, lastExpr) <- letsFromComplexExp expr
      varName <- Utils.freshVarName fresh
      let exprType = Ast.TypedAst.typeFromTExpr expr
      pure (letStmts ++ [TLet exprType varName lastExpr, TPrint ty (TVar exprType varName)])
    TLet ty binding expr -> do
      (stmts, lastExpr) <- letsFromComplexExp expr
      pure (stmts ++ [TLet ty binding lastExpr])

-- Creates let statements from complex expressions
letsFromComplexExp :: TypedExpr -> StateErrorRndEff Context Text ([TypedStmt], TypedExpr)
letsFromComplexExp expr' =
  case expr' of
    expr | isReduced expr -> pure ([], expr)
    TUnaryOp ty op expr -> createLetBinding expr (TUnaryOp ty op)
    TBinOp ty op exprL exprR | isAtomic exprL -> createLetBinding exprR (TBinOp ty op exprL)
    TBinOp ty op exprL exprR | isAtomic exprR -> createLetBinding exprL $ flip (TBinOp ty op) exprR
    TBinOp ty op exprL exprR -> createDoubleLetBinding exprL exprR (TBinOp ty op)
    expr -> pure ([], expr)

-- | Creates a single let statement, it will have the shape of tmp_x
--   where x is an incremental number
createLetBinding :: TypedExpr -> (TypedExpr -> TypedExpr) -> StateErrorRndEff Context Text ([TypedStmt], TypedExpr)
createLetBinding expr expConstr = do
  varName <- Utils.freshVarName fresh
  (stmts, expr') <- letsFromComplexExp expr
  let exprType = Ast.TypedAst.typeFromTExpr expr
  pure (stmts ++ [TLet exprType varName expr'], expConstr (TVar exprType varName))

-- | Utility function to create two let bindings at one from one
createDoubleLetBinding :: TypedExpr -> TypedExpr -> (TypedExpr -> TypedExpr -> TypedExpr) -> StateErrorRndEff Context Text ([TypedStmt], TypedExpr)
createDoubleLetBinding exprL exprR expConstr = do
  varNameL <- Utils.freshVarName fresh
  varNameR <- Utils.freshVarName fresh
  (stmtsL, exprL') <- letsFromComplexExp exprL
  (stmtsR, exprR') <- letsFromComplexExp exprR
  let exprType = Ast.TypedAst.typeFromTExpr exprL
  pure (stmtsL ++ [TLet exprType varNameL exprL'] ++ stmtsR ++ [TLet exprType varNameR exprR'], expConstr (TVar exprType varNameL) (TVar exprType varNameR))

-- If it's reduced it means that it can't be reduced further
isReduced :: TypedExpr -> Bool
isReduced expr | isAtomic expr = True
isReduced (TBinOp _ _ expr1 expr2) = isAtomic expr1 && isAtomic expr2
isReduced (TUnaryOp _ _ expr) = isAtomic expr
isReduced _ = False

isAtomic :: TypedExpr -> Bool
isAtomic (TConst _ _) = True
isAtomic (TVar _ _) = True
isAtomic _ = False
