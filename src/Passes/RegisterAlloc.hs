{-# OPTIONS_GHC -Wno-unused-matches #-}

module Passes.RegisterAlloc where

import           Ast.Ast
import           Control.Carrier.State.Strict
import           Data.Set                       ( Set
                                                , difference
                                                , empty
                                                , fromList
                                                , union
                                                )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import           GeneralDS.Graph                ( Graph
                                                , defaultNode
                                                , insertEdge
                                                , insertNode
                                                , newGraph
                                                )
import qualified Passes.PassEffs               as PassEffs

{-
  # Register Allocation

  The register allocator will build liveness analysis and an interference graph to
  find which variables can use a register insted of stack var mapping.

  ## Liveness Analysis

  Definition:
      A variable is live at a program point if the value in the variable
      is used at some later point in the program.

  Each line will save the liveness after (i.e. on k+1) since that's what's used for
  the interference graph.

  The liveness analysis is processed backwards with the following equations.

  L_after(k) = L_before(k + 1)
  L_after(n) = {}
  L_before(k) = (L_after(k) - W(k)) U R(k)

  e.g. with x86 var pseudocode

                        {}
      movq $1, v
                        {v}
      movq $42, w
                        {v,w}
      movq v, x
                        {w,x}
      addq $7, x
                        {w,x}
      movq x, y
                        {w,x,y}
      movq x, z
                        {w,y,z}
      addq w, z
                        {y,z}
      movq y, t
                        {t,z}
      negq t
                        {t,z}
      movq z, %rax
                        {t}
      addq t, %rax
                        {}
      jmp conclusion
                        {}

 ## Interference Graph

   Definition:
      undirected graph whose vertices represent variables and whose edges represent conflicts,
      i.e., when two vertices are live at the same time.

   The naive approach of inspecting all the livenessAfter set of each statement is generally O(n^2)
   Also, something like `movq x, y  {w,x,y}` would mark x and y to be conflictive but since it's the
   same value they can share the same register.

   To make it faster we use heuristics based on writes, check

-}

-- Caller-save registers (the ones that a procedure can use freely):  rax rdx rcx rsi rdi r8 r9 r10 r11
-- Callee-save registers (the ones that a procedure has to save and restore if used):
--   rsp rbp rbx r12 r13 r14 r15

-- Available Registers (rax is used as a tmp register) rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14

type LivenessBefore = Set Text

type WritesK = Set Text

type LivenessAfter = Set Text

type ReadsK = Set Text

-- Storing only the livenessAfter per stmt since that's what's going to be used in the interference graph
data EnrichedStmt = EnrichedStmt
  { livenessAfter :: LivenessAfter
  , stmt          :: Stmt
  }
  deriving (Show, Eq)

-- Caller-save registers (the ones that a procedure can use freely): rax rdx rcx rsi rdi r8 r9 r10 r11
callerSavedRegisters :: Set Text
callerSavedRegisters =
  Set.fromList ["rax", "rdx", "rcx", "rsi", "rdi", "r8", "r9", "r10", "r11"]

allocRegisters :: Program -> PassEffs.StErr sig m Program
allocRegisters program = do
  locals <- gets infoLocals
  let stmts         = progStmts program
  let enrichedStmts = buildLiveness stmts
  let interfGraph   = buildInterferenceGraph locals enrichedStmts
  undefined

-- Private --

buildInterferenceGraph :: Locals -> [EnrichedStmt] -> Graph Text
buildInterferenceGraph vars enrichedStmts =
  let nodes          = fmap defaultNode (Set.toList vars)
      graphWithNodes = foldr insertNode newGraph nodes
  in  foldr applyInterfHeuristics graphWithNodes enrichedStmts

-- | We add an edge between each write and the rest of livenessAfter
-- 1. For a let stmt (Let d (Var s)), for each v in (livenessAfter / {d, s}),
--    add edge (d,v)

-- 2. For an other non move stmts, for each v in (livenessAfter / writeSet),
--    add edge (d,v)

-- TODO: No call yet
-- 3. For a call instruction, for each v in L_after,
--    for each r in caller-save registers if r != v then add edge (r,v)

-- 4. For a print instruction, for each v in L_after,
--    for each r in {rdi, rsi, rax} then add edge (r,v)

-- TODO test it!
applyInterfHeuristics :: EnrichedStmt -> Graph Text -> Graph Text
-- Move var to var statement
applyInterfHeuristics EnrichedStmt { livenessAfter, stmt = Let var (Var var') } graph
  = let otherLiveVars =
          livenessAfter `Set.difference` (Set.fromList [var, var'])
    in  foldr (\liveVar graphAcc -> insertEdge (var, liveVar) graphAcc)
              graph
              (Set.toList otherLiveVars)
-- Non move statement, i.e. some expression result assigned to a var
applyInterfHeuristics EnrichedStmt { livenessAfter, stmt = Let var _expr } graph
  = let otherLiveVars = livenessAfter `Set.difference` (Set.fromList [var])
    in  foldr (\liveVar graphAcc -> insertEdge (var, liveVar) graphAcc)
              graph
              (Set.toList otherLiveVars)
-- print Statemet, we added the used registers by the print call (we should add
-- caller-saved registers but we all)
applyInterfHeuristics EnrichedStmt { livenessAfter, stmt = Print expr } graph =
  foldr
    (\liveVar graphAcc -> foldr
      (\reg graphAcc' -> insertEdge (reg, liveVar) graphAcc')
      graphAcc
      callerSavedRegisters
    )
    graph
    (Set.toList livenessAfter)
applyInterfHeuristics EnrichedStmt { livenessAfter, stmt } graph = graph

buildLiveness :: [Stmt] -> [EnrichedStmt]
buildLiveness stmts = snd $ foldr reducer (empty, []) stmts
 where
  reducer :: Stmt -> (Set Text, [EnrichedStmt]) -> (Set Text, [EnrichedStmt])
  reducer stmt (livenessAfter, enrichedStmts) =
    let liveness     = buildStmtLiveness stmt livenessAfter
        enrichedStmt = EnrichedStmt livenessAfter stmt
    in  (liveness, enrichedStmt : enrichedStmts)


buildStmtLiveness :: Stmt -> LivenessAfter -> LivenessBefore
buildStmtLiveness (Let binding expr) livenessAfter =
  let writeSet = fromList [binding]
  in  livenessBefore livenessAfter writeSet (readsFromExpr expr)
buildStmtLiveness stmt@(Return expr) livenessAfter =
  livenessBefore livenessAfter empty (readsFromExpr expr)
buildStmtLiveness stmt@(Print expr) livenessAfter =
  livenessBefore livenessAfter empty (readsFromExpr expr)

readsFromExpr :: Expr -> Set Text
readsFromExpr (Const _       ) = empty
readsFromExpr (Var   binding ) = fromList [binding]
readsFromExpr (UnaryOp _ expr) = readsFromExpr expr
readsFromExpr (BinOp _ expr expr') =
  readsFromExpr expr `union` readsFromExpr expr'

-- L_before(k) = (L_after(k) - W(k)) U R(k)
-- where W(k) means writes for line k and R(k) means for reads for linke k
livenessBefore :: LivenessAfter -> WritesK -> ReadsK -> Set Text
livenessBefore livenessAfter writesK readsK =
  (livenessAfter `difference` writesK) `union` readsK
