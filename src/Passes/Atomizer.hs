-- TypedAtomizer
--
-- removes complex expressions in statments
-- and transforms them in TAC version
--
-- Works on a Program directly
--
-- e.g.
-- let x = (1 + (2 + (3 + 4)))
-- =>
-- let y = (2 + (3 + 4)); let x = 1 + y
-- =>
-- let z = 3 + 4; let y = 2 + z; let x = 1 + y
module Passes.Atomizer where

import Ast.Ast
import qualified Ast.Ast as Ast
import Ast.PrettyPrinting
import Context (Context, addLocal)
import Control.Carrier.Error.Either
import Control.Carrier.Fresh.Strict
import Control.Carrier.State.Strict
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text.Read as T
import EffUtils (StateErrorEff, StateErrorEffM, StateErrorRndEff, StateErrorRndEffM)
import Irs.AtomIr
import Types.Defs (Type)
import Utils hiding (liftEither)

-- Extracts the Context and Program from the effects
runRemComplexStmts :: Context -> Program Expr Type -> Either Text (Context, Program AExpr Type)
runRemComplexStmts info program = run . runError . runState info $ removeComplexStmts program

--  === Public Api ===

removeComplexStmts :: Program Expr Type -> StateErrorEffM Context Text m (Program AExpr Type)
removeComplexStmts (Program stmts) = fmap snd $
    runFresh 0 $ do
        stmts' <- foldl' reducer (pure []) stmts
        addBindsToContext stmts'
        pure (Program stmts')
  where
    reducer :: StateErrorRndEffM Context Text m [Stmt AExpr Type] -> Stmt Expr Type -> StateErrorRndEffM Context Text m [Stmt AExpr Type]
    reducer prevStmts stmt = do
        prevStmts' <- prevStmts
        stmts' <- removeComplexStmt stmt
        pure (prevStmts' ++ stmts')

-- === Private ====

addBindsToContext :: [Stmt AExpr Type] -> StateErrorEff Context Text ()
addBindsToContext = mapM_ mapper
  where
    mapper :: Stmt AExpr Type -> StateErrorEff Context Text ()
    mapper (Let _ binding _) = modify @Context (Context.addLocal binding)
    mapper _ = pure ()

--  Transform a complex statment (i.e. statements that are not atomic) into sequential let bindings
removeComplexStmt :: Stmt Expr Type -> StateErrorRndEff Context Text [Stmt AExpr Type]
removeComplexStmt stmt =
    case stmt of
        stmt'@(Return ty (Lit lit)) -> pure [Return ty (ALit lit)]
        stmt'@(Print ty (Lit lit)) -> pure [Print ty (ALit lit)]
        stmt'@(Let ty bind (Lit lit)) -> pure [Let ty bind (ALit lit)]
        Return ty expr -> do
            (letStmts, lit) <- letsFromComplexExp expr
            varName <- Utils.freshVarName fresh
            pure (letStmts ++ [Let ty varName lit, Return ty (ALit (LVar ty varName))])

-- Print ty expr -> do
--     (letStmts, lastExpr) <- letsFromComplexExp expr
--     varName <- Utils.freshVarName fresh
--     let exprType = Ast.typeFromExpr expr
--     pure (letStmts ++ [Let exprType varName lastExpr, Print ty (ALit (LVar exprType varName))])
-- Let ty binding expr -> do
--     (stmts, lastExpr) <- letsFromComplexExp expr
--     pure (stmts ++ [Let ty binding lastExpr])

-- Creates let statements from complex expressions
letsFromComplexExp :: Expr Type -> StateErrorRndEff Context Text ([Stmt AExpr Type], AExpr Type)
letsFromComplexExp expr' =
    case expr' of
        UnaryOp ty op (Lit lit) -> pure ([], AUnaryOp ty op lit)
        BinOp ty op (Lit litL) (Lit litR) -> pure ([], ABinOp ty op litL litR)
        UnaryOp ty op expr -> createLetBinding expr (AUnaryOp ty op)
        BinOp ty op (Lit lit) exprR -> createLetBinding exprR (ABinOp ty op lit)
        BinOp ty op exprL (Lit lit) -> createLetBinding exprL $ flip (ABinOp ty op) lit
        BinOp ty op exprL exprR -> createDoubleLetBinding exprL exprR (ABinOp ty op)

{- | Creates a single let statement, it will have the shape of tmp_x
  where x is an incremental number
-}
createLetBinding :: Expr Type -> (Literal Type -> AExpr Type) -> StateErrorRndEff Context Text ([Stmt AExpr Type], AExpr Type)
createLetBinding expr' expConstr = do
    expr <- toAExpr expr'
    varName <- Utils.freshVarName fresh
    (stmts, expr'') <- letsFromComplexExp expr'
    let exprType = Ast.typeFromExpr expr'
    pure (stmts ++ [Let exprType varName expr''], expConstr (LVar exprType varName))

-- | Utility function to create two let bindings at one from one
createDoubleLetBinding :: Expr Type -> Expr Type -> (Literal Type -> Literal Type -> AExpr Type) -> StateErrorRndEff Context Text ([Stmt AExpr Type], AExpr Type)
createDoubleLetBinding exprL exprR expConstr = do
    varNameL <- Utils.freshVarName fresh
    varNameR <- Utils.freshVarName fresh
    (stmtsL, exprL') <- letsFromComplexExp exprL
    (stmtsR, exprR') <- letsFromComplexExp exprR
    let exprType = Ast.typeFromExpr exprL
    pure (stmtsL ++ [Let exprType varNameL exprL'] ++ stmtsR ++ [Let exprType varNameR exprR'], expConstr (LVar exprType varNameL) (LVar exprType varNameR))

toAExpr :: Expr Type -> StateErrorRndEff Context Text (AExpr Type)
toAExpr (Lit lit) = pure $ ALit lit
toAExpr expr = throwError $ "Failure atomizing expr: " <> prettyPrint expr <> " for some reason didn't end up being atomic"
