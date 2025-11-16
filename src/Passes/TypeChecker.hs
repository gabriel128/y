{-# LANGUAGE OverloadedStrings #-}

module Passes.TypeChecker where

-- \| Type Checker
--
-- Converts a program with expressions and maybe types to a program that's typed iff the
-- program is a valid `Y` program

import Ast.Ast
import qualified Ast.Ast as Ast
import Ast.PrettyPrinting ()
import Context (Context)
import Control.Carrier.Error.Church (liftEither)
import Data.Either.Combinators (maybeToRight)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import EffUtils (StateErrorEffM)
import Types.Defs (Type (..))
import Utils (PrettyPrint (prettyPrint))

type VarToTypeMappings = M.Map Text Type

typeCheck :: Program Expr (Maybe Type) -> StateErrorEffM Context Text m (Program Expr Type)
typeCheck typedProgram = do
    let stmts = progStmts typedProgram
    let varToTypesMap = M.empty
    (_, inferredStmts) <- liftEither $ foldl' reducer (Right (varToTypesMap, [])) stmts
    -- let untypedStmts = fmap from stmts
    pure (Ast.newProgram (reverse inferredStmts))
  where
    reducer :: Either Text (VarToTypeMappings, [Stmt Expr Type]) -> Stmt Expr (Maybe Type) -> Either Text (VarToTypeMappings, [Stmt Expr Type])
    reducer acc stmt = do
        (typeMap', typedStmts) <- acc
        case typeCheckStmt stmt typeMap' of
            Right (x, tstmt) -> Right (x, tstmt : typedStmts)
            Left err -> Left $ "Type check failed for => " <> prettyPrint stmt <> "\nErr => " <> err

typeCheckStmt :: Stmt Expr (Maybe Type) -> VarToTypeMappings -> Either Text (VarToTypeMappings, Stmt Expr Type)
typeCheckStmt stmt typeMap =
    case stmt of
        (Return _ expr) -> do
            typedExpr <- inferExpr expr typeMap
            let ty = typeFromExpr typedExpr
            Right (typeMap, Return ty typedExpr)
        (Print ty expr) -> do
            typedExpr <- inferExpr expr typeMap
            Right (typeMap, Print ty typedExpr)
        (Let Nothing label expr) -> do
            typedExpr <- inferExpr expr typeMap
            let ty = typeFromExpr typedExpr
            let newMap = M.insert label ty typeMap
            Right (newMap, Let ty label typedExpr)
        (Let (Just letType) label expr) -> do
            typedExpr <- inferExpr expr typeMap
            let castedExpr = castExprIfNativeInt typedExpr letType
            let ty = typeFromExpr castedExpr
            _ <- isSameTyWithErr letType ty (prettyPrint letType <> " doesn't match with " <> prettyPrint ty <> ". Duh!")
            let newMap = M.insert label letType typeMap
            Right (newMap, Let letType label castedExpr)

-- TODO add linenumbers
inferExpr :: Expr (Maybe Type) -> VarToTypeMappings -> Either Text (Expr Type)
inferExpr texpr typeMap =
    case texpr of
        (Lit (LNum ty val)) -> Right (Lit (LNum ty val))
        (Lit (LBool ty val)) -> Right (Lit (LBool ty val))
        (Lit (LVar Nothing label)) -> do
            ty <- maybeToRight ("Can't infer type for " <> prettyPrint label <> ", are you sure you declared it? :|") $ M.lookup label typeMap
            Right . Lit $ LVar ty label
        (Lit (LVar (Just ty) label)) -> pure (Lit (LVar ty label))
        UnaryOp Nothing Ast.Not _expr' -> Left "Not implemented yet"
        UnaryOp Nothing Ast.Neg expr' -> do
            typedExpr <- inferExpr expr' typeMap
            case typeFromExpr typedExpr of
                ty | ty `elem` [I64, U64] -> Right (UnaryOp ty Ast.Neg typedExpr)
                _otherwise -> Left $ "Negation only take numeric types, found: " <> prettyPrint typedExpr
        (UnaryOp (Just t) op expr) -> do
            typedExpr <- inferExpr expr typeMap
            Right $ UnaryOp t op typedExpr
        (BinOp Nothing op leftExpr rightExpr) -> do
            leftExpr' <- inferExpr leftExpr typeMap
            rightExpr' <- inferExpr rightExpr typeMap
            let leftType = typeFromExpr leftExpr'
            let rightType = typeFromExpr rightExpr'
            _ <-
                isSameTyWithErr
                    leftType
                    rightType
                    ( prettyPrint leftExpr'
                        <> ":"
                        <> prettyPrint leftType
                        <> " is not the same type as "
                        <> prettyPrint rightExpr'
                        <> ":"
                        <> prettyPrint rightType
                        <> ". Duh!"
                    )
            _ <- typeCheckBinOp op leftType
            _ <- typeCheckBinOp op rightType
            _ <- checkDiv0 op rightExpr'
            opType <- inferBinOp op leftType rightType
            Right $ BinOp opType op leftExpr' rightExpr'
        (BinOp (Just ty) op lexpr rexpr) -> do
            -- TODO: Check all the stuff like above
            rightExpr <- inferExpr rexpr typeMap
            leftExpr <- inferExpr lexpr typeMap
            _ <- checkDiv0 op rightExpr
            Right $ BinOp ty op leftExpr rightExpr

-- | Cast native expressions usueful for cases like `let x : u64 = 8;`, 8 will be u64
castExprIfNativeInt :: Expr Type -> Type -> Expr Type
castExprIfNativeInt lit@(Lit (LNum ty x)) ty'
    | ty == ty' = lit
    | otherwise = Lit (LNum ty' x)
castExprIfNativeInt expr _ = expr

typeCheckBinOp :: BinOp -> Type -> Either Text ()
typeCheckBinOp binop nativeTy
    | binop `elem` [Ast.Add, Ast.Sub, Ast.Mul, Ast.Div, Ast.ShiftL] && nativeTy `elem` [I64, U64] = Right ()
    | binop `elem` [Ast.Le, Ast.Lt, Ast.Eq, Ast.Neq] && nativeTy == TyBool = Right ()
typeCheckBinOp binop ty = Left $ "type " <> prettyPrint ty <> " can't be handled by " <> prettyPrint binop

{- | Infers binop final type.
    - I64 will be chosen over U64 if any of the *hs is signed
    - if any of the *hs is mutable the inferred type will be mutable as well
TODO:
- add mutability automatic cast
-}
inferBinOp :: BinOp -> Type -> Type -> Either Text Type
inferBinOp _ lty rty | lty == I64 || rty == I64 = Right I64
inferBinOp _ lty rty | lty == U64 || rty == U64 = Right U64
inferBinOp binop lty rty = Left $ "Can not infer " <> prettyPrint lty <> " " <> prettyPrint binop <> " " <> prettyPrint rty

-- | Division by zero is type checked if we know that the rhs is zero at typechecking type
checkDiv0 :: BinOp -> Expr Type -> Either Text ()
checkDiv0 Ast.Div (Lit (LNum _ 0)) = Left $ T.pack "Can not divide by zero you idiot"
checkDiv0 _ _ = Right ()

-- ensureInferred :: Expr -> Either Text ()
-- ensureInferred typedExpr =
--   case typeFromTExpr typedExpr of
--     TyToInfer -> Left $ T.pack ("Couldn't infer: " <> show typedExpr <> " my bad!")
--     _ -> Right ()

isSameTyWithErr :: Type -> Type -> Text -> Either Text ()
isSameTyWithErr tx ty _ | tx == ty = Right ()
isSameTyWithErr _ _ err = Left err
