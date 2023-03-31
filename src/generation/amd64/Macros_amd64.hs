module Macros_amd64 (exitMacroAmd64, printMacro) where

import Data (addString)
import Data.Binary (Word32)
import Instructions_amd64 (Register (RA, RD, RDI, RSI), leaRipRel, movImm, syscall)
import Relocation (RelocatableWriter)

exitMacroAmd64 :: Word32 -> RelocatableWriter ()
exitMacroAmd64 exitCode = do
  movImm RDI exitCode
  movImm RA 60
  syscall

printMacro :: String -> RelocatableWriter ()
printMacro str = do
  (lab, len) <- addString str
  movImm RA 1
  movImm RDI 1
  leaRipRel RSI lab
  movImm RD (fromIntegral len)
  syscall