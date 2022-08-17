{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- | Represents type checked X86 in NASM format
module Irs.X86 where

import qualified Irs.Nasm as Nasm

newtype X86Program = X86Program [Nasm.Instr]
