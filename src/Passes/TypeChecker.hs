module Passes.TypeChecker where

-- \|
--
-- Type Checker
--
-- This type checker can be placed before or after the Atomizer.
-- Doing type checking after the atomizer ensures that typechecking statements
-- is O(n). However having it after will require workarounds for type errors since
-- the atomizer will create temp variables so for now assume that this is going
-- to be the first thing that runs.

import Ast.Ast (BinOp (..), Expr (BinOp, Const, UnaryOp, Var), Program (progStmts), Stmt (Let, Print, Return), UnaryOp (Neg))
import Context (Context)
import Control.Carrier.Error.Church (liftEither)
import Data.Either.Combinators (maybeToRight)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import EffUtils (StateErrorEffM)
import Types.Defs (NativeType (..), Type (Native, TyToInfer))

type VarToTypeMappings = M.Map Text Type

typeCheck :: Program -> StateErrorEffM Context Text m Program
typeCheck program = do
  let stmts = progStmts program
  let varToTypesMap = M.empty
  _ <- liftEither $ foldl' reducer (Right varToTypesMap) stmts
  pure program
  where
    reducer :: Either Text VarToTypeMappings -> Stmt -> Either Text VarToTypeMappings
    reducer typeMap stmt = do
      typeMap' <- typeMap
      typeCheckStmt stmt typeMap'

typeCheckStmt :: Stmt -> VarToTypeMappings -> Either Text VarToTypeMappings
typeCheckStmt stmt typeMap =
  case stmt of
    Return expr -> do
      _ <- getExprType expr typeMap
      Right typeMap
    Print expr -> do
      _ <- getExprType expr typeMap
      Right typeMap
    Let TyToInfer label expr -> do
      exprType <- getExprType expr typeMap
      let newMap = M.insert label exprType typeMap
      Right newMap
    Let letType label expr -> do
      exprType <- getExprType expr typeMap
      if letType == exprType
        then do
          let newMap = M.insert label letType typeMap
          Right newMap
        else Left $ T.pack ("type check failed for var definition on line x: " <> show letType <> " doesn't match with " <> show exprType)

-- TODO add linenumbers
getExprType :: Expr -> VarToTypeMappings -> Either Text Type
getExprType expr typeMap =
  case expr of
    Const nativeType _val -> Right . Native $ nativeType
    Var TyToInfer label -> do
      maybeToRight (T.pack ("Can't infer type for" <> show label)) $ M.lookup label typeMap
    Var ty _ -> Right ty
    UnaryOp Neg expr' -> do
      theType <- getExprType expr' typeMap
      if theType `elem` [Native I64, Native I64]
        then Right theType
        else Left $ T.pack ("Negation only take numeric types, found: " <> show theType)
    BinOp op leftExpr rightExpr -> do
      leftType <- getExprType leftExpr typeMap
      rightType <- getExprType rightExpr typeMap
      if leftType == rightType
        then do
          _ <- typeCheckBinOp op leftType
          Right leftType
        else Left $ T.pack ("type check failed for " <> show op <> "on line x: lhs " <> show leftType <> "doesn't match with rhs " <> show rightType)

typeCheckBinOp :: BinOp -> Type -> Either Text ()
typeCheckBinOp binop (Native nativeTy)
  | binop `elem` [Add, Sub, Mul, Div, ShiftL] && nativeTy `elem` [I64, U64] = Right ()
typeCheckBinOp binop ty = Left $ T.pack $ "type " <> show ty <> " can't be handled by " <> show binop
