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

type VarToTypeMappings = M.Map Text Type

typeCheck :: TypedProgram -> StateErrorEffM Context Text m Program
typeCheck typedProgram = do
  let stmts = typedProgStmts typedProgram
  let varToTypesMap = M.empty
  _ <- liftEither $ foldl' reducer (Right varToTypesMap) stmts
  let untypedStmts = fmap from stmts
  pure (Ast.newProgram untypedStmts)
  where
    reducer :: Either Text VarToTypeMappings -> TypedStmt -> Either Text VarToTypeMappings
    reducer typeMap stmt = do
      typeMap' <- typeMap
      case typeCheckStmt stmt typeMap' of
        Right x -> Right x
        Left err -> Left $ T.pack $ "Type check failed for " <> show stmt <> "on line {x}: " <> show err

typeCheckStmt :: TypedStmt -> VarToTypeMappings -> Either Text VarToTypeMappings
typeCheckStmt tstmt typeMap =
  case tstmt of
    (TReturn _ expr) -> do
      _ <- getExprType expr typeMap
      Right typeMap
    (TPrint _ expr) -> do
      _ <- getExprType expr typeMap
      Right typeMap
    (TLet TyToInfer label expr) -> do
      exprType <- getExprType expr typeMap
      let newMap = M.insert label exprType typeMap
      Right newMap
    (TLet letType label expr) -> do
      exprType <- getExprType expr typeMap
      if sameTypeIgnoreMut letType exprType
        then do
          let newMap = M.insert label letType typeMap
          Right newMap
        else Left $ T.pack ("type check failed for var definition on line x: " <> show letType <> " doesn't match with " <> show exprType <> ". Duh!")

-- TODO add linenumbers
getExprType :: TypedExpr -> VarToTypeMappings -> Either Text Type
getExprType texpr typeMap =
  case texpr of
    TConst ty _val -> Right ty
    TVar TyToInfer label -> do
      maybeToRight
        (T.pack ("Can't infer type for " <> show label <> ", are you sure you declared it? :|"))
        $ M.lookup label typeMap
    TVar ty _ -> Right ty
    TUnaryOp TyToInfer Ast.Neg expr' -> do
      theType <- getExprType expr' typeMap
      case theType of
        (TyNative _ a) | a `elem` [I64, U64] -> Right theType
        _otherwise -> Left $ T.pack ("Negation only take numeric types, found: " <> show theType)
    TUnaryOp ty _ _ -> Right ty
    TBinOp TyToInfer op leftExpr rightExpr -> do
      leftType <- getExprType leftExpr typeMap
      rightType <- getExprType rightExpr typeMap
      _ <- typeCheckBinOp op leftType
      _ <- typeCheckBinOp op rightType
      _ <- checkDiv0 op rightExpr
      opType <- inferBinOp op leftType rightType
      Right opType
    TBinOp ty op _ rightExpr -> do
      _ <- checkDiv0 op rightExpr
      Right ty

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
