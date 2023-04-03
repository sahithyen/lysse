module Routines_amd64 (printChar) where

import Code_amd64 (label)
import Data (addDWord)
import Instructions_amd64 (Register (RA, RD, RDI, RSI), leaRipRel, movImm, syscall)
import Relocation (RelocatableWriter)

-- RDI = ascii char
printChar :: RelocatableWriter ()
printChar = do
  label "printChar"
  buf <- addDWord 0
  leaRipRel RSI buf
  --

  movImm RA 1
  movImm RDI 1

  movImm RD 1
  syscall