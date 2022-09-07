{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
import Data.Text (Text, pack)
import Nasm.Data
import qualified Nasm.Data as Nasm
import Nasm.Dsl
import qualified Passes.PassEffs as PassEffs

type LocalStackMap = M.Map Text MemDeref

astToNasm :: Program -> PassEffs.StErr sig m [Nasm.Instr]
astToNasm prog = do
  localVars <- gets @Info infoLocals
  let (stackOffset, stackVars) = mapVarsToStack localVars
  put (Info localVars (alignStack16 stackOffset))
  (_, instrs) <- runState @LocalStackMap stackVars $ fromStmtsToInstrs (progStmts prog)
  pure instrs

alignStack16 :: Int -> Int
alignStack16 offset = offset - (offset `mod` 16)

lookupEither :: Text -> LocalStackMap -> Either Text MemDeref
lookupEither binding mapping = maybeToRight ("Var not bound: " <> binding) (M.lookup binding mapping)

mapVarsToStack :: [Text] -> (Offset, LocalStackMap)
mapVarsToStack = foldr reducer (0, M.empty)
  where
    reducer :: Text -> (Offset, LocalStackMap) -> (Offset, LocalStackMap)
    reducer local (n, amap) =
      let amap' = M.insert local (MemDeref Rbp (n - 8)) amap
       in (n - 8, amap')

getStackMapping :: (Has (State LocalStackMap) sig m, (Has (Throw Text) sig m)) => Text -> m MemDeref
getStackMapping binding = gets (lookupEither binding) >>= liftEither >>= pure

fromStmtsToInstrs :: (Has (State LocalStackMap) sig m, (Has (Throw Text) sig m)) => [Stmt] -> m [Instr]
fromStmtsToInstrs = foldl' reducer (pure [])
  where
    reducer instrs stmt = do
      prevInstrs <- instrs
      newInstrs <- fromStmtToInstrs stmt
      pure $ prevInstrs ++ newInstrs

fromStmtToInstrs :: (Has (State LocalStackMap) sig m, (Has (Throw Text) sig m)) => Stmt -> m [Instr]
-- x = 3;
fromStmtToInstrs (Let binding (Const num)) = do
  x <- getStackMapping binding
  pure [movmi x num]
-- x = y; -> mov rax y; mov x rax
fromStmtToInstrs (Let binding (Var binding2)) = do
  x <- getStackMapping binding
  y <- getStackMapping binding2
  pure [movrm Rax y, movmr x Rax]
-- return 4;
fromStmtToInstrs (Return (Const num)) =
  pure [movri Rax num, ret]
-- return x;
fromStmtToInstrs (Return (Var binding)) =
  getStackMapping binding >>= (\x -> pure [movrm Rax x, ret])
-- print 3
fromStmtToInstrs (Print (Const num)) =
  pure [movrl Rdi "printf_format", movri Rsi num, xor Rax Rax, call "printf WRT ..plt"]
-- print x
fromStmtToInstrs (Print (Var binding)) =
  getStackMapping binding >>=
  (\x -> pure [movrl Rdi "printf_format", movrm Rsi x, xor Rax Rax, call "printf WRT ..plt"])
-- Handle addition
-- x = 2 + 2; -> mov x, 2; add x, 2
fromStmtToInstrs (Let binding (BinOp Ast.Add (Const num1) (Const num2))) = do
  x <- getStackMapping binding
  pure [movmi x num1, addmi x num2]
-- x = 2 + y; -> mov rax, y; add rax, 2; mov x rax
-- x = 2 + x; -> add x, 2
fromStmtToInstrs (Let binding (BinOp Ast.Add (Const num) (Var binding2))) = do
  x <- getStackMapping binding
  y <- getStackMapping binding2
  if binding == binding2
    then pure [addmi x 2]
    else pure [movrm Rax y, addri Rax num, movmr x Rax]
-- Add is commutative so we just call the above definition
fromStmtToInstrs (Let binding (BinOp Ast.Add (Var binding2) (Const num))) =
  fromStmtToInstrs (Let binding (BinOp Ast.Add (Const num) (Var binding2)))
-- x = z + y; -> mov rax, z; add rax, y; mov x, rax
fromStmtToInstrs (Let binding (BinOp Ast.Add (Var binding1) (Var binding2))) = do
  x <- getStackMapping binding
  y <- getStackMapping binding1
  z <- getStackMapping binding2
  pure [movrm Rax z, addrm Rax y, movmr x Rax]

-- Handle substaction
-- x = 2 - 2 -> mov x, 2; sub x, 2
fromStmtToInstrs (Let binding (BinOp Ast.Sub (Const num1) (Const num2))) = do
  x <- getStackMapping binding
  pure [movmi x num1, submi x num2]
-- x = 2 - y -> mov rax, 2; sub rax, y; mov x rax
fromStmtToInstrs (Let binding (BinOp Ast.Sub (Const num) (Var binding2))) = do
  x <- getStackMapping binding
  y <- getStackMapping binding2
  pure [movri Rax num, subrm Rax y, movmr x Rax]
-- x = y - 2 -> mov rax, y; sub rax, 2; mov x rax
fromStmtToInstrs (Let binding (BinOp Ast.Sub (Var binding2) (Const num))) = do
  x <- getStackMapping binding
  y <- getStackMapping binding2
  pure [movrm Rax y, subri Rax num, movmr x Rax]
-- x = z - y -> mov rax, z; sub rax, y; mov x, rax
fromStmtToInstrs (Let binding (BinOp Ast.Sub (Var binding1) (Var binding2))) = do
  x <- getStackMapping binding
  y <- getStackMapping binding1
  z <- getStackMapping binding2
  pure [movrm Rax z, subrm Rax y, movmr x Rax]

-- Unhandled
fromStmtToInstrs stmt = throwError (pack $ "Unhandled stmt: " <> show stmt)
