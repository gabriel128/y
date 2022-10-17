{-# LANGUAGE QuasiQuotes #-}

module Passes.X86ToTextProg where

import Ast.Ast (Info (infoStackOffset))
import Control.Carrier.State.Strict
import Data.List (intersperse)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Nasm.Data as Nasm
import NeatInterpolation
import qualified Passes.PassEffs as PassEffs
import Utils

instrsToText :: [Nasm.Instr] -> PassEffs.StErr sig m Text
instrsToText intrs = do
  stackOffset <- gets infoStackOffset
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
