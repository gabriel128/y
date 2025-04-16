{-# OPTIONS_GHC -Wno-unused-matches #-}

module Optimizations.RegisterAlloc where

import Ast.Ast
import Context (Context)
import Data.Set (Set, difference, empty, fromList, union)
import Data.Text (Text)
import EffUtils (StateErrorEff)

-- Available Registers  rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14

type LivenessBefore = Set Text

type WritesK = Set Text

type LivenessAfterK = Set Text

type ReadsK = Set Text

newtype StmtMetadata = StmtMetadata {liveness :: Set Text} deriving (Show, Eq)

data EnrichedStmt = EnrichedStmt {stmtMetadata :: StmtMetadata, stmt :: Stmt} deriving (Show, Eq)

allocRegisters :: Program -> StateErrorEff Context Text Program
allocRegisters = undefined

buildLiveness :: [Stmt] -> [EnrichedStmt]
buildLiveness stmts = snd $ foldr reducer (empty, []) stmts
  where
    reducer :: Stmt -> (Set Text, [EnrichedStmt]) -> (Set Text, [EnrichedStmt])
    reducer stmt (livenessAfter, enrichedStmts) =
      let liveness = buildStmtLiveness stmt livenessAfter
          enrichedStmt = EnrichedStmt (StmtMetadata livenessAfter) stmt
       in (liveness, enrichedStmt : enrichedStmts)

buildStmtLiveness :: Stmt -> LivenessAfterK -> LivenessBefore
buildStmtLiveness (Let binding expr) livenessAfter =
  livenessBeforeK livenessAfter (fromList [binding]) (readsFromExpr expr)
buildStmtLiveness stmt@(Return expr) livenessAfter =
  livenessBeforeK livenessAfter empty (readsFromExpr expr)
buildStmtLiveness stmt@(Print expr) livenessAfter =
  livenessBeforeK livenessAfter empty (readsFromExpr expr)

readsFromExpr :: Expr -> Set Text
readsFromExpr (Const _) = empty
readsFromExpr (Var binding) = fromList [binding]
readsFromExpr (UnaryOp _ expr) = readsFromExpr expr
readsFromExpr (BinOp _ expr expr') = readsFromExpr expr `union` readsFromExpr expr'

-- L_after(n) = {}
-- L_before(k)  = (L_after(k) - W(k)) \union R(k)
livenessBeforeK :: LivenessAfterK -> WritesK -> ReadsK -> Set Text
livenessBeforeK lafter writesK readsK = (lafter `difference` writesK) `union` readsK
