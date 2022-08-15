{-# LANGUAGE BangPatterns #-}

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
module Irs.Atomize (removeComplexStmts) where

import Ast.Ast
import Data.Foldable
import System.Random (RandomGen)
import Utils

removeComplexStmts :: Info -> [Stmt] -> Program
removeComplexStmts info rawStmts = snd $ foldl' reducer (createRandomGen, Program info []) rawStmts
  where
    reducer (gen, Program info' stmts) stmt =
      let (gen', Program info'' stmts') = removeComplexStmt info' gen stmt
       in (gen', Program info'' (stmts ++ stmts'))

removeComplexStmt :: RandomGen g => Info -> g -> Stmt -> (g, Program)
removeComplexStmt info randomGen (Let binding expr) = go (removeComplexExp info randomGen expr)
  where
    go (gen, info', stmts, expr') = (gen, Program info' (stmts ++ [Let binding expr']))
removeComplexStmt info gen stmt = (gen, Program info [stmt])

-- (BinOp Add (Const 10) (UnaryOp Neg (Const 8))
-- ->
-- ([Let var (UnaryOp Neg (Const 8))], (BinOp Add (Const 10) (Var var1)))

-- (UnaryOp Neg (UnaryOp Neg (Const 8)))
-- ->
-- [Let var (UnaryOp Neg (Const 8))], UnaryOp Neg (Var var1)]
removeComplexExp :: RandomGen g => Info -> g -> Expr -> (g, Info, [Stmt], Expr)
removeComplexExp info gen expr | isReduced expr = (gen, info, [], expr)
removeComplexExp info gen (UnaryOp op expr) =
  let (varName, newGen) = Utils.randVarName gen
      (newGen', info', stmts, expr') = removeComplexExp info newGen expr
   in (newGen', addLocal varName info', stmts ++ [Let varName expr'], UnaryOp op (Var varName))
removeComplexExp info gen (BinOp op exprL exprR)
  | isAtomic exprL =
    let (varName, newGen) = Utils.randVarName gen
        (newGen', info', stmts, exprR') = removeComplexExp info newGen exprR
     in (newGen', addLocal varName info', stmts ++ [Let varName exprR'], BinOp op exprL (Var varName))
  | isAtomic exprR =
    let (varName, newGen) = Utils.randVarName gen
        (newGen', info', stmts, exprL') = removeComplexExp info newGen exprL
     in (newGen', addLocal varName info', stmts ++ [Let varName exprL'], BinOp op (Var varName) exprR)
  | otherwise =
    let (varNameL, newGen) = Utils.randVarName gen
        (varNameR, newGen') = Utils.randVarName newGen
        (newGen'', info', stmtsL, exprL') = removeComplexExp info newGen' exprL
        (newGen''', info'', stmtsR, exprR') = removeComplexExp (addLocal varNameL info') newGen'' exprR
     in (newGen''', addLocal varNameR info'', stmtsL ++ [Let varNameL exprL'] ++ stmtsR ++ [Let varNameR exprR'], BinOp op (Var varNameL) (Var varNameR))
removeComplexExp info gen expr = (gen, info, [], expr)

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
