{-# LANGUAGE QuasiQuotes #-}

module Passes.X86ToTextProg where

import Control.Carrier.State.Strict
import Data.List (intersperse)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Nasm.Data as Nasm
import NeatInterpolation
import EffUtils (StateErrorRndEff, StateErrorEff)
import Utils
import qualified Context
import Context (Context)

instrsToText :: [Nasm.Instr] -> StateErrorEff Context Text Text
instrsToText intrs = do
  stackOffset <- gets Context.ctxStackOffset
  let textInstrs = printInstrs intrs
  pure (programTemplate textInstrs stackOffset)

printInstrs :: [Nasm.Instr] -> Text
printInstrs instrs = T.concat $ intersperse " \n" (fmap textPrint instrs)

programTemplate :: Text -> Int -> Text
programTemplate logic stackOffset =
  let offset = pack $ show stackOffset
   in [trimming|

      extern printf

      global main

      section .data
        printf_format: db '%d',10,0

      section .text

      main:
          push rbp
          mov  rbp, rsp
          add  rsp, $offset
          call start
          call conclusion

      start:
        $logic

      conclusion:
          mov rsp, rbp
          pop rbp
          ret
    |]
