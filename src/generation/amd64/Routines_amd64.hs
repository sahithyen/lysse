module Routines_amd64 (routines) where

import Code_amd64 (label)
import Data (addDWord)
import Data.Foldable (sequenceA_)
import Instructions_amd64 (Register (RA, RBP, RD, RDI, RSI, RSP), addImm32, call, indirectStore, leaRipRel, movImm, pop, push, ret, syscall)
import Relocation (RelocatableWriter)

routines :: RelocatableWriter ()
routines = sequenceA_ [printChar, printDigit]

-- RDI = ascii char
printChar :: RelocatableWriter ()
printChar = do
  label "printChar"
  buf <- addDWord 0

  push RBP
  leaRipRel RSI buf
  indirectStore RSI RDI

  movImm RA 1
  movImm RDI 1

  movImm RD 1
  syscall

  pop RBP

  ret

-- RDI = number between 0-9
printDigit :: RelocatableWriter ()
printDigit = do
  label "printDigit"

  addImm32 RDI 48
  -- call "printChar"

  ret