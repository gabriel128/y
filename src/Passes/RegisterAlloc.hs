{-# OPTIONS_GHC -Wno-unused-matches #-}

module Passes.RegisterAlloc where

import Ast.Ast
import Data.Set (Set, difference, empty, fromList, union)
import Data.Text (Text)
import qualified Passes.PassEffs as PassEffs

{-
  # Register Allocation

  The register allocator will build liveness analysis and an interference graph to
  find which variables can use a register insted of stack var mapping.

  ## Liveness Analysis

  The liveness analysis is processed backwards with the following equations

  L(k) = (L(k+1) - W(k)) U R(k)

  e.g. with x86 var pseudocode

    movq $1, v        {}
    movq $42, w       {v}
    movq v, x         {v,w}
    addq $7, x        {w,x}
    movq x, y         {w,x}
    movq x, z         {w,x,y}
    addq w, z         {w,y,z}
    movq y, t         {y,z}
    negq t            {t,z}
    movq z, %rax      {t,z}
    addq t, %rax      {t}
    jmp conclusion    {}
-}

-- Caller-save registers (the ones that a procedure can use freely):  rax rdx rcx rsi rdi r8 r9 r10 r11
-- Callee-save registers (the ones that a procedure has to save and restore if used):
--   rsp rbp rbx r12 r13 r14 r15

-- Available Registers (rax is used as a tmp register) rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14

type LivenessK = Set Text

type WritesK = Set Text

type LivenessKplus1 = Set Text

type ReadsK = Set Text

newtype StmtMetadata = StmtMetadata {liveness :: Set Text} deriving (Show, Eq)

data EnrichedStmt = EnrichedStmt {stmtMetadata :: StmtMetadata, stmt :: Stmt} deriving (Show, Eq)

allocRegisters :: Program -> PassEffs.StErr sig m Program
allocRegisters = undefined

buildLiveness :: [Stmt] -> [EnrichedStmt]
buildLiveness stmts = snd $ foldr reducer (empty, []) stmts
  where
    reducer :: Stmt -> (Set Text, [EnrichedStmt]) -> (Set Text, [EnrichedStmt])
    reducer stmt (livenessKplus1, enrichedStmts) =
      let liveness = buildStmtLiveness stmt livenessKplus1
          enrichedStmt = EnrichedStmt (StmtMetadata liveness) stmt
       in (liveness, enrichedStmt : enrichedStmts)

-- Insights:
-- in a let expression
buildStmtLiveness :: Stmt -> LivenessKplus1 -> LivenessK
buildStmtLiveness (Let binding expr) livenessKplus1 =
  livenessK livenessKplus1 (fromList [binding]) (readsFromExpr expr)
buildStmtLiveness stmt@(Return expr) livenessKplus1 =
  livenessK livenessKplus1 empty (readsFromExpr expr)
buildStmtLiveness stmt@(Print expr) livenessKplus1 =
  livenessK livenessKplus1 empty (readsFromExpr expr)

readsFromExpr :: Expr -> Set Text
readsFromExpr (Const _) = empty
readsFromExpr (Fn _) = empty
readsFromExpr (Var binding) = fromList [binding]
readsFromExpr (UnaryOp _ expr) = readsFromExpr expr
readsFromExpr (BinOp _ expr expr') = readsFromExpr expr `union` readsFromExpr expr'

--  L(k) = (L(k+1) - W(k)) U R(k)
-- where W(k) means writes for line k and R(k) means for reads for linke k
livenessK :: LivenessKplus1 -> WritesK -> ReadsK -> Set Text
livenessK livenessKplus1 writesK readsK = (livenessKplus1 `difference` writesK) `union` readsK
