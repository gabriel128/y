{-# LANGUAGE FlexibleContexts #-}

-- | Represents type checked X86 in NASM format
module Passes.StmtsToX86 where

import Ast.Ast
import qualified Ast.Ast as Ast
-- import Data.Either.Combinators (maybeToRight)

import Control.Carrier.Error.Either
import Control.Carrier.State.Strict
import Data.Either.Combinators (maybeToRight)
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Nasm.Data as Nasm
import Context (Context (..))
import qualified Context
import EffUtils (StateErrorEff)

-- $setup

-- Map from variables to stack offets
type LocalStackMap = M.Map Text MemDeref

astToNasm :: Program -> StateErrorEff Context Text [Nasm.Instr]
astToNasm prog = do
  localVars <- gets @Context Context.localsList
  let (stackOffset, varsStackMapping) = mapVarsToBspOffset localVars
  put (Context (Set.fromList localVars) (alignStack16 stackOffset))
  (_, instrs) <- runState @LocalStackMap varsStackMapping $ fromStmtsToInstrs (progStmts prog)
  pure instrs

-- == Private ==

--- | As per ABI requirements the stack must be aligned 16bytes before any call.
-- More info see https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf Section 3.2.2
alignStack16 :: Int -> Int
alignStack16 offset = offset - (offset `mod` 16)

lookupEither :: Text -> LocalStackMap -> Either Text MemDeref
lookupEither binding mapping = maybeToRight ("Var not bound: " <> binding) (M.lookup binding mapping)

-- | Assigns a base pointer offset to each variable
-- >>> mapVarsToBspOffset ["x", "y"]
-- (-16,fromList [("x",Deref Rbp (-8)),("y",Deref Rbp (-16))])
-- >>> mapVarsToBspOffset ["x"]
-- (-8,fromList [("x",Deref Rbp (-8))])
mapVarsToBspOffset :: [Text] -> (Offset, LocalStackMap)
mapVarsToBspOffset = foldr reducer (0, M.empty) . reverse
  where
    reducer :: Text -> (Offset, LocalStackMap) -> (Offset, LocalStackMap)
    reducer local (n, amap) =
      let amap' = M.insert local (Deref Rbp (n - 8)) amap
       in (n - 8, amap')

getStackMapping :: (Has (State LocalStackMap) sig m, (Has (Throw Text) sig m)) => Text -> m MemDeref
getStackMapping binding = gets (lookupEither binding) >>= liftEither

fromStmtsToInstrs :: (Has (State LocalStackMap) sig m, (Has (Throw Text) sig m)) => [Stmt] -> m [Instr]
fromStmtsToInstrs = foldl' reducer (pure [])
  where
    reducer instrs stmt = do
      prevInstrs <- instrs
      newInstrs <- fromStmtToInstrs stmt
      pure $ prevInstrs ++ newInstrs

fromStmtToInstrs :: (Has (State LocalStackMap) sig m, (Has (Throw Text) sig m)) => Stmt -> m [Instr]
-- let x = 3;
fromStmtToInstrs (Let binding (Const num)) = do
  x <- getStackMapping binding
  pure [Mov x num]

-- x = y; -> mov rax y; mov x rax
fromStmtToInstrs (Let binding (Var binding2)) = do
  x <- getStackMapping binding
  y <- getStackMapping binding2
  pure [Mov Rax y, Mov x Rax]
-- return 4;
fromStmtToInstrs (Return (Const num)) =
  pure [Mov Rax num, Ret]
-- return x;
fromStmtToInstrs (Return (Var binding)) =
  getStackMapping binding >>= (\x -> pure [Mov Rax x, Ret])
-- print 3
fromStmtToInstrs (Print (Const num)) =
  pure [Mov Rdi printFormatLabel, Mov Rsi num, Xor Rax Rax, Call "printf WRT ..plt"]
-- print x
fromStmtToInstrs (Print (Var binding)) =
  getStackMapping binding
    >>= (\x -> pure [Mov Rdi printFormatLabel, Mov Rsi x, Xor Rax Rax, Call "printf WRT ..plt"])
-- Handle addition
-- x = 2 + 2; -> mov x, 2; add x, 2
fromStmtToInstrs (Let binding (BinOp Ast.Add (Const num1) (Const num2))) = do
  x <- getStackMapping binding
  pure [Mov x num1, Nasm.Add x num2]
-- x = 2 + y; -> mov rax, y; add rax, 2; mov x rax
-- x = 2 + x; -> add x, 2
fromStmtToInstrs (Let binding (BinOp Ast.Add (Const num) (Var binding2))) = do
  x <- getStackMapping binding
  y <- getStackMapping binding2
  let z = (2 :: Int)
  if binding == binding2
    then pure [Nasm.Add x z]
    else pure [Mov Rax y, Nasm.Add Rax num, Mov x Rax]
-- Add is commutative so we just call the above definition
fromStmtToInstrs (Let binding (BinOp Ast.Add (Var binding2) (Const num))) =
  fromStmtToInstrs (Let binding (BinOp Ast.Add (Const num) (Var binding2)))
-- x = z + y; -> mov rax, z; add rax, y; mov x, rax
fromStmtToInstrs (Let binding (BinOp Ast.Add (Var binding1) (Var binding2))) = do
  x <- getStackMapping binding
  y <- getStackMapping binding1
  z <- getStackMapping binding2
  pure [Mov Rax z, Nasm.Add Rax y, Mov x Rax]

-- Handle substaction
-- x = 2 - 2 -> mov x, 2; sub x, 2
fromStmtToInstrs (Let binding (BinOp Ast.Sub (Const num1) (Const num2))) = do
  x <- getStackMapping binding
  pure [Mov x num1, Nasm.Sub x num2]
-- x = 2 - y -> mov rax, 2; sub rax, y; mov x rax
fromStmtToInstrs (Let binding (BinOp Ast.Sub (Const num) (Var binding2))) = do
  x <- getStackMapping binding
  y <- getStackMapping binding2
  pure [Mov Rax num, Nasm.Sub Rax y, Mov x Rax]
-- x = y - 2 -> mov rax, y; sub rax, 2; mov x rax
fromStmtToInstrs (Let binding (BinOp Ast.Sub (Var binding2) (Const num))) = do
  x <- getStackMapping binding
  y <- getStackMapping binding2
  pure [Mov Rax y, Nasm.Sub Rax num, Mov x Rax]
-- x = z - y -> mov rax, z; sub rax, y; mov x, rax
fromStmtToInstrs (Let binding (BinOp Ast.Sub (Var binding1) (Var binding2))) = do
  x <- getStackMapping binding
  y <- getStackMapping binding1
  z <- getStackMapping binding2
  pure [Mov Rax z, Nasm.Sub Rax y, Mov x Rax]

-- -- Unhandled
fromStmtToInstrs stmt = throwError (pack $ "Unhandled stmt: " <> show stmt)
