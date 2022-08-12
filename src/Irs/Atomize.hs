{-# LANGUAGE BangPatterns #-}

-- |
-- Atomizer
--
-- removes complex expressions in statments
--
-- e.g.
-- let x = (1 + (2 + (3 + 4)))
-- =>
-- let y = (2 + (3 + 4)); let x = 1 + y
-- =>
-- let z = 3 + 4; let y = 2 + z; let x = 1 + y
module Irs.Atomize (removeComplexStmt) where

import Ast.Ast
import System.Random (RandomGen)
import Utils

removeComplexStmt :: Stmt -> [Stmt]
removeComplexStmt (Let binding expr) = go (removeComplexExp createRandomGen expr)
  where
    go (stmts, expr') = stmts ++ [Let binding expr']
removeComplexStmt stmt = [stmt]

-- (BinOp Add (Const 10) (UnaryOp Neg (Const 8))
-- ->
-- ([Let var (UnaryOp Neg (Const 8))], (BinOp Add (Const 10) (Var var1)))

-- (UnaryOp Neg (UnaryOp Neg (Const 8)))
-- ->
-- [Let var (UnaryOp Neg (Const 8))], UnaryOp Neg (Var var1)]
removeComplexExp :: RandomGen g => g -> Expr -> ([Stmt], Expr)
removeComplexExp _ expr | isReduced expr = ([], expr)
removeComplexExp gen (UnaryOp op expr) =
  let (varName, newGen) = Utils.randVarName gen
      (stmts, expr') = removeComplexExp newGen expr
   in (stmts ++ [Let varName expr'], UnaryOp op (Var varName))
removeComplexExp gen (BinOp op exprL exprR)
  | isAtomic exprL =
    let (varName, newGen) = Utils.randVarName gen
        (stmts, exprR') = removeComplexExp newGen exprR
     in (stmts ++ [Let varName exprR'], BinOp op exprL (Var varName))
  | isAtomic exprR =
    let (varName, newGen) = Utils.randVarName gen
        (stmts, exprL') = removeComplexExp newGen exprL
     in (stmts ++ [Let varName exprL'], BinOp op (Var varName) exprR)
  | otherwise =
    let (varNameL, newGen) = Utils.randVarName gen
        (varNameR, newGen') = Utils.randVarName newGen
        (stmtsL, exprL') = removeComplexExp newGen exprL
        (stmtsR, exprR') = removeComplexExp newGen' exprR
     in (stmtsL ++ [Let varNameL exprL'] ++ stmtsR ++ [Let varNameR exprR'], BinOp op (Var varNameL) (Var varNameR))
removeComplexExp _ expr = ([], expr)

isAtomic :: Expr -> Bool
isAtomic (Const _) = True
isAtomic (Var _) = True
isAtomic _ = False

isReduced :: Expr -> Bool
isReduced (Const _) = True
isReduced (Var _) = True
isReduced (BinOp _ (Const _) (Const _)) = True
isReduced (BinOp _ (Var _) (Const _)) = True
isReduced (BinOp _ (Const _) (Var _)) = True
isReduced (BinOp _ (Var _) (Var _)) = True
isReduced (UnaryOp _ (Const _)) = True
isReduced (UnaryOp _ (Var _)) = True
isReduced _ = False
