{-# LANGUAGE ImportQualifiedPost #-}

module Routines_amd64 (routines) where

import Code (createLabel, label)
import Code_amd64 qualified as C (stacked)
import Data (addDWord)
import Data.Foldable (sequenceA_)
import Instructions_amd64 (Register (RA, RBP, RD, RDI, RSI), addImm32, call, cmp, imul, indirectStore, jle, jns, leaRipRel, movImm, neg, pop, push, ret, syscall, test)
import Macros_amd64 (printMacro)
import Relocation (RelocatableWriter)

routines :: RelocatableWriter ()
routines = sequenceA_ [printChar, printDigit, printNumber]

-- RDI = signed 64 bit number
printNumber :: RelocatableWriter ()
printNumber = do
  label "printNumber"

  -- If negative print the sign and apply two's complement
  positive <- createLabel
  test RDI RDI
  jns positive
  neg RDI
  C.stacked [RDI] $ printMacro "-"
  label positive

  -- movImm RDI 100

  -- movImm RA 10
  -- movImm RD 10

  -- maxDigitLoop <- createLabel
  -- label maxDigitLoop

  -- C.stacked [RA, RD, RDI] $ printMacro "@"

  -- imul RD

  -- cmp RDI RA
  -- jle maxDigitLoop

  movImm RDI 1000

  movImm RA 10
  movImm RD 10
  imul RD

  exit1 <- createLabel
  cmp RA RDI
  jle exit1

  movImm RDI 0
  movImm RA 60
  syscall

  label exit1

  movImm RDI 1
  movImm RA 60
  syscall

  ret

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
  call "printChar"
  ret