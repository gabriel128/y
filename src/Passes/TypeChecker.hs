{-# LANGUAGE OverloadedStrings #-}

module Passes.TypeChecker where

-- \| Type Checker
--
-- Converts a TypedProgram to a valid Program. In other words if a program type checks is
-- considered valid

import Ast.Ast
import qualified Ast.Ast as Ast
import Ast.PrettyPrinting
import Context (Context)
import Control.Carrier.Error.Church (liftEither)
import Data.Either.Combinators (maybeToRight)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import EffUtils (StateErrorEffM)
import Types.Defs (NativeType (..), Type (..))
import Utils (PrettyPrint (prettyPrint))

type VarToTypeMappings = M.Map Text Type

typeCheck :: Program -> StateErrorEffM Context Text m Program
typeCheck typedProgram = do
    let stmts = progStmts typedProgram
    let varToTypesMap = M.empty
    (_, inferredStmts) <- liftEither $ foldl' reducer (Right (varToTypesMap, [])) stmts
    -- let untypedStmts = fmap from stmts
    pure (Ast.newProgram (reverse inferredStmts))
  where
    reducer :: Either Text (VarToTypeMappings, [Stmt]) -> Stmt -> Either Text (VarToTypeMappings, [Stmt])
    reducer acc stmt = do
        (typeMap', typedStmts) <- acc
        case typeCheckStmt stmt typeMap' of
            Right (x, tstmt) -> Right (x, tstmt : typedStmts)
            Left err -> Left $ "Type check failed for => " <> prettyPrint stmt <> "\nErr => " <> err

typeCheckStmt :: Stmt -> VarToTypeMappings -> Either Text (VarToTypeMappings, Stmt)
typeCheckStmt tstmt typeMap =
    case tstmt of
        (Return _ expr) -> do
            typedExpr <- getExpr expr typeMap
            let ty = typeFromExpr typedExpr
            Right (typeMap, Return ty typedExpr)
        (Print ty expr) -> do
            typedExpr <- getExpr expr typeMap
            Right (typeMap, Print ty typedExpr)
        (Let TyToInfer label expr) -> do
            typedExpr <- getExpr expr typeMap
            let ty = typeFromExpr typedExpr
            let newMap = M.insert label ty typeMap
            Right (newMap, Let ty label typedExpr)
        (Let letType label expr) -> do
            typedExpr <- getExpr expr typeMap
            let castedExpr = castExprIfNativeInt typedExpr letType
            let ty = typeFromExpr castedExpr
            _ <- isSameTyWithErr letType ty (prettyPrint letType <> " doesn't match with " <> prettyPrint ty <> ". Duh!")
            let newMap = M.insert label letType typeMap
            Right (newMap, Let letType label castedExpr)

-- TODO add linenumbers
getExpr :: Expr -> VarToTypeMappings -> Either Text Expr
getExpr texpr typeMap =
    case texpr of
        tconst@(Const _ty _val) -> Right tconst
        Var TyToInfer label -> do
            ty <- maybeToRight ("Can't infer type for " <> prettyPrint label <> ", are you sure you declared it? :|") $ M.lookup label typeMap
            Right $ Var ty label
        tvar@(Var _ty _) -> do
            Right tvar
        UnaryOp TyToInfer Ast.Neg expr' -> do
            typedExpr <- getExpr expr' typeMap
            case typeFromExpr typedExpr of
                ty@(MkNativeType a) | a `elem` [I64, U64] -> Right (UnaryOp ty Ast.Neg typedExpr)
                _otherwise -> Left $ "Negation only take numeric types, found: " <> prettyPrint typedExpr
        tunary@(UnaryOp{}) -> Right tunary
        binop@(BinOp TyToInfer op leftExpr rightExpr) -> do
            leftExpr <- getExpr leftExpr typeMap
            rightExpr <- getExpr rightExpr typeMap
            let leftType = typeFromExpr leftExpr
            let rightType = typeFromExpr rightExpr
            _ <-
                isSameTyWithErr
                    leftType
                    rightType
                    ( "("
                        <> prettyPrint leftExpr
                        <> "):"
                        <> prettyPrint leftType
                        <> " is not the same type as ("
                        <> prettyPrint rightExpr
                        <> "):"
                        <> prettyPrint rightType
                        <> ". Duh!"
                    )
            _ <- typeCheckBinOp op leftType
            _ <- typeCheckBinOp op rightType
            _ <- checkDiv0 op rightExpr
            opType <- inferBinOp op leftType rightType
            Right $ BinOp opType op leftExpr rightExpr
        tbinop@(BinOp _ op _ rightExpr) -> do
            _ <- checkDiv0 op rightExpr
            Right tbinop

-- | Cast native expressions usueful for cases like `let x : u64 = 8;`, 8 will be u64
castExprIfNativeInt :: Expr -> Type -> Expr
castExprIfNativeInt const@(Const nType x) (MkNativeType nType')
    | nType == nType' = const
    | nType `elem` [I64, U64] && nType' `elem` [I64, U64] = Const nType' x
castExprIfNativeInt expr _ = expr

typeCheckBinOp :: BinOp -> Type -> Either Text ()
typeCheckBinOp binop (MkNativeType nativeTy)
    | binop `elem` [Ast.Add, Ast.Sub, Ast.Mul, Ast.Div, Ast.ShiftL] && nativeTy `elem` [I64, U64] = Right ()
    | binop `elem` [Ast.Le, Ast.Eq, Ast.Lt] && nativeTy == TyBool = Right ()
typeCheckBinOp binop ty = Left $ "type " <> prettyPrint ty <> " can't be handled by " <> prettyPrint binop

{- | Infers binop final type.
    - I64 will be chosen over U64 if any of the *hs is signed
    - if any of the *hs is mutable the inferred type will be mutable as well
TODO:
- add mutability automatic cast
-}
inferBinOp :: BinOp -> Type -> Type -> Either Text Type
inferBinOp _ (MkNativeType lty) (MkNativeType rty) | lty == I64 || rty == I64 = Right (MkNativeType I64)
inferBinOp _ (MkNativeType _) (MkNativeType _) = Right (MkNativeType U64)
inferBinOp binop lty rty = Left $ "Can not infer " <> prettyPrint lty <> " " <> prettyPrint binop <> " " <> prettyPrint rty

-- | Division by zero is type checked if we know that the rhs is zero at typechecking type
checkDiv0 :: BinOp -> Expr -> Either Text ()
checkDiv0 Ast.Div (Const _ "0") = Left $ T.pack "Can not divide by zero you idiot"
checkDiv0 _ _ = Right ()

-- ensureInferred :: Expr -> Either Text ()
-- ensureInferred typedExpr =
--   case typeFromTExpr typedExpr of
--     TyToInfer -> Left $ T.pack ("Couldn't infer: " <> show typedExpr <> " my bad!")
--     _ -> Right ()

isSameTyWithErr :: Type -> Type -> Text -> Either Text ()
isSameTyWithErr tx ty _ | tx == ty = Right ()
isSameTyWithErr _ _ err = Left err
