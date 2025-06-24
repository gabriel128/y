module Passes.TypeChecker where

-- \| Type Checker
--
-- Converts a TypedProgram to a valid Program. In other words if a program type checks is
-- considered valid

import Ast.Ast (BinOp, Program)
import qualified Ast.Ast as Ast
import Ast.TypedAst
import Context (Context)
import Control.Carrier.Error.Church (liftEither)
import Data.Either.Combinators (maybeToRight)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import EffUtils (StateErrorEffM)
import Types.Defs (NativeType (..), Type (..), mkImmNativeType, sameTypeIgnoreMut)
import qualified Ast.TypedAst as Ast

type VarToTypeMappings = M.Map Text Type

typeCheck :: TypedProgram -> StateErrorEffM Context Text m TypedProgram
typeCheck typedProgram = do
  let stmts = typedProgStmts typedProgram
  let varToTypesMap = M.empty
  (_, inferredTypedStmts) <- liftEither $ foldl' reducer (Right (varToTypesMap, [])) stmts
  -- let untypedStmts = fmap from stmts
  pure (Ast.newTypedProgram (reverse inferredTypedStmts))
  where
    reducer :: Either Text (VarToTypeMappings, [TypedStmt]) -> TypedStmt -> Either Text (VarToTypeMappings, [TypedStmt])
    reducer acc stmt = do
      (typeMap', typedStmts) <- acc
      case typeCheckStmt stmt typeMap' of
        Right (x, tstmt) -> Right (x, tstmt : typedStmts)
        Left err -> Left $ T.pack $ "Type check failed for " <> show stmt <> "on line {x}: " <> show err

typeCheckStmt :: TypedStmt -> VarToTypeMappings -> Either Text (VarToTypeMappings, TypedStmt)
typeCheckStmt tstmt typeMap =
  case tstmt of
    (TReturn _ expr) -> do
      typedExpr <- getTypedExpr expr typeMap
      let ty = typeFromTExpr typedExpr
      Right (typeMap, TReturn ty typedExpr)
    (TPrint ty expr) -> do
      typedExpr <- getTypedExpr expr typeMap
      Right (typeMap, TPrint ty typedExpr)
    (TLet TyToInfer label expr) -> do
      typedExpr <- getTypedExpr expr typeMap
      let ty = typeFromTExpr typedExpr
      let newMap = M.insert label ty typeMap
      Right (newMap, TLet ty label typedExpr)
    (TLet letType label expr) -> do
      typedExpr <- getTypedExpr expr typeMap
      let ty = typeFromTExpr typedExpr
      if sameTypeIgnoreMut letType ty
        then do
          let newMap = M.insert label letType typeMap
          Right (newMap, TLet letType label typedExpr)
        else Left $ T.pack ("type check failed for var definition on line x: " <> show letType <> " doesn't match with " <> show typedExpr <> ". Duh!")

-- TODO add linenumbers
getTypedExpr :: TypedExpr -> VarToTypeMappings -> Either Text TypedExpr
getTypedExpr texpr typeMap =
  case texpr of
    tconst@(TConst _ty _val) -> Right tconst
    TVar TyToInfer label -> do
      ty <- maybeToRight (T.pack ("Can't infer type for " <> show label <> ", are you sure you declared it? :|")) $ M.lookup label typeMap
      Right $ TVar ty label
    tvar@(TVar _ty _) -> do
      Right tvar
    TUnaryOp TyToInfer Ast.Neg expr' -> do
      typedExpr <- getTypedExpr expr' typeMap
      case typeFromTExpr typedExpr of
        ty@(TyNative _ a) | a `elem` [I64, U64] -> Right (TUnaryOp ty Ast.Neg typedExpr)
        _otherwise -> Left $ T.pack ("Negation only take numeric types, found: " <> show typedExpr)
    tunary@(TUnaryOp {}) -> Right tunary
    TBinOp TyToInfer op leftExpr rightExpr -> do
      leftTypedExpr <- getTypedExpr leftExpr typeMap
      rightTypedExpr <- getTypedExpr rightExpr typeMap
      let leftType = typeFromTExpr leftTypedExpr
      let rightType = typeFromTExpr rightTypedExpr
      _ <- typeCheckBinOp op leftType
      _ <- typeCheckBinOp op rightType
      _ <- checkDiv0 op rightExpr
      opType <- inferBinOp op leftType rightType
      Right $ TBinOp opType op leftTypedExpr rightTypedExpr
    tbinop@(TBinOp _ op _ rightExpr) -> do
      _ <- checkDiv0 op rightExpr
      Right tbinop

-- ensureInferred :: TypedExpr -> Either Text ()
-- ensureInferred typedExpr =
--   case typeFromTExpr typedExpr of
--     TyToInfer -> Left $ T.pack ("Couldn't infer: " <> show typedExpr <> " my bad!")
--     _ -> Right ()

typeCheckBinOp :: BinOp -> Type -> Either Text ()
typeCheckBinOp binop (TyNative _ nativeTy)
  | binop `elem` [Ast.Add, Ast.Sub, Ast.Mul, Ast.Div, Ast.ShiftL] && nativeTy `elem` [I64, U64] = Right ()
typeCheckBinOp binop ty = Left $ T.pack $ "type " <> show ty <> " can't be handled by " <> show binop

-- | Infers binop final type.
--     - I64 will be chosen over U64 if any of the *hs is signed
--     - if any of the *hs is mutable the inferred type will be mutable as well
-- TODO:
-- - add mutability automatic cast
inferBinOp :: BinOp -> Type -> Type -> Either Text Type
inferBinOp _ (TyNative _ lty) (TyNative _ rty) | lty == I64 || rty == I64 = Right (mkImmNativeType I64)
inferBinOp _ (TyNative _ _) (TyNative _ _) = Right (mkImmNativeType U64)
inferBinOp binop lty rty = Left $ T.pack $ "Can not infer " <> show lty <> " " <> show binop <> " " <> show rty

-- | Division by zero is type checked if we know that the rhs is zero at typechecking type
checkDiv0 :: BinOp -> TypedExpr -> Either Text ()
checkDiv0 Ast.Div (TConst _ (Ast.NativeInt 0)) = Left $ T.pack "Can not divide by zero you idiot"
checkDiv0 _ _ = Right ()

-- \| binop `elem` [Ast.Add, Ast.Sub, Ast.Mul, Ast.Div, Ast.ShiftL] && nativeTy `elem` [I64, U64] = Right ()
