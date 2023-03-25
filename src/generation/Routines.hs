module Routines (routines) where

import Code (call, createLabel, label, stacked)
import Data (addDWord)
import Data.Foldable (sequenceA_)
import Instructions
  ( addi,
    adr,
    b,
    bcond,
    cmp,
    lr,
    lsli,
    madd,
    mov,
    movzx,
    msub,
    neg,
    r0,
    r1,
    r2,
    r3,
    r4,
    r8,
    ret,
    sdiv,
    stri,
    subi,
    svc,
    tbz,
    wzr,
  )
import Macros (printMacro)
import Relocation (RelocatableWriter)

routines :: RelocatableWriter ()
routines = sequenceA_ [printNumber, printChar, printDigit, printBinary]

-- r0 = signed 64 bit number
printNumber :: RelocatableWriter ()
printNumber = do
  label "printNumber"

  -- If negative print the sign and apply two's complement
  positive <- createLabel
  tbz r0 63 positive
  neg r0 r0
  stacked [r0, lr] $ printMacro "-"
  label positive

  -- Find divisor for largest digit (divisor is in r2)
  movzx r1 10
  movzx r2 10
  maxDigitLoop <- createLabel
  label maxDigitLoop
  madd r1 r1 r2 wzr
  cmp r1 r0
  bcond 0xd maxDigitLoop
  sdiv r1 r1 r2

  -- If beginning number smaller than 10 then we only need to print one digit
  lastDigit <- createLabel
  cmp r1 r0
  bcond 0xc lastDigit

  digitLoop <- createLabel
  label digitLoop

  -- Quotient is current digit and remainder is for the next digits
  -- r0 dividend
  -- r1 divisor
  -- r2 quotient
  -- r3 remainder
  sdiv r2 r0 r1
  msub r3 r2 r1 r0

  -- Print digit (quotient)
  mov r0 r2
  call [r1, r3, lr] "printDigit"

  -- Move remainder for next digits
  mov r0 r3

  -- Calculate divisor for next digit
  movzx r2 10
  sdiv r1 r1 r2

  -- Is not the last digit if the divisor is greater than 1
  movzx r4 1
  cmp r1 r4
  bcond 0xc digitLoop

  -- Print last digit
  label lastDigit
  call [lr] "printDigit"

  printMacro "\n"

  ret lr

-- r0 = ascii char
printChar :: RelocatableWriter ()
printChar = do
  label "printChar"
  buf <- addDWord 0
  adr r1 buf
  stri r0 r1 0
  movzx r0 1
  movzx r2 1
  movzx r8 64
  svc 0
  ret lr

-- r0 = number between 0-9
printDigit :: RelocatableWriter ()
printDigit = do
  label "printDigit"
  addi r0 r0 48
  b "printChar"

-- r0 = 64 bit number
printBinary :: RelocatableWriter ()
printBinary = do
  label "printBinary"

  movzx r1 64

  loop <- createLabel
  label loop

  zero <- createLabel
  movzx r3 0
  tbz r0 63 zero
  movzx r3 1
  label zero

  stacked [r0] $ do
    mov r0 r3
    call [r1, lr] "printDigit"

  lsli r0 r0 1 -- left shift
  subi r1 r1 1
  movzx r2 0
  cmp r1 r2
  bcond 0x1 loop

  printMacro "\n"

  ret lr