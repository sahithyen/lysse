module LyGen (lysseProgram) where

import Data (addDWord, addLabeledDWord, addString)
import Data.Binary (Word32)
import Instructions
  ( add,
    addi,
    adr,
    b,
    bcond,
    bl,
    cmp,
    ldri,
    ldrlx,
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
    sp,
    stri,
    stripre,
    sub,
    subi,
    svc,
    tbz,
    wzr,
  )
import Relocation (RelocatableWriter, addLabel)
import STree (LAExpression (..), LAIdentifier (LAIdentifier), LAStatement (..), getIdentifiers)

lysseProgram :: [LAStatement] -> RelocatableWriter ()
lysseProgram stmts = do
  printNumber
  printBinary
  addLabel "code" "_start"

  allocator stmts
  statementsW stmts

  movzx r0 0
  exitRoutine

-- Allocator
allocator :: [LAStatement] -> RelocatableWriter ()
allocator stmts = mapM_ allocateIdentifier $ getIdentifiers stmts

allocateIdentifier :: String -> RelocatableWriter ()
allocateIdentifier ident = addLabeledDWord (getIdentifierLabel ident) 0

getIdentifierLabel :: String -> String
getIdentifierLabel ident = "ident@" ++ ident

-- Writer
statementsW :: [LAStatement] -> RelocatableWriter ()
statementsW = mapM_ statementW

statementW :: LAStatement -> RelocatableWriter ()
statementW (LAInput ident) = error "n/a"
statementW (LAOutput (LAIdentifier ident)) = do
  printRoutine $ ident ++ " = "
  ldrlx r0 $ getIdentifierLabel ident
  bl "printNumber"
statementW (LAAssignment (LAIdentifier ident) expr) = do
  expressionW r0 expr
  adr r1 $ getIdentifierLabel ident
  stri r0 r1 0

expressionW :: Word32 -> LAExpression -> RelocatableWriter ()
expressionW rd (LAAddition a_op b_op) =
  withSpareRegister rd $ \rb ->
    do
      expressionW rd a_op
      expressionW rb b_op
      add rd rd rb
expressionW rd (LASubtraction a_op b_op) =
  withSpareRegister rd $ \rb ->
    do
      expressionW rd a_op
      expressionW rb b_op
      sub rd rd rb
expressionW rd (LAReference (LAIdentifier ident)) = ldrlx rd $ getIdentifierLabel ident
expressionW rd (LAInteger a) =
  do
    l <- addDWord a
    ldrlx rd l

withSpareRegister :: Word32 -> (Word32 -> RelocatableWriter ()) -> RelocatableWriter ()
withSpareRegister r fn =
  if r < 8
    then fn $ r + 1
    else
      ( do
          stripre (r - 1) sp (-16)
          fn (r - 1)
          ldri (r - 1) sp 16
      )

-- Routines

-- r0 exit code
exitRoutine :: RelocatableWriter ()
exitRoutine = do
  movzx r8 93
  svc 0

printNumber :: RelocatableWriter ()
printNumber = do
  addLabel "code" "printNumber"
  addLabeledDWord "printNumber_buf" 0

  tbz r0 63 "positive"

  neg r0 r0

  stripre r0 sp (-16)
  stripre lr sp (-16)
  printRoutine "-"
  ldri lr sp 16
  ldri r0 sp 16

  addLabel "code" "positive"

  movzx r1 10
  movzx r2 10
  addLabel "code" "loop0"
  madd r1 r1 r2 wzr
  cmp r1 r0
  bcond 0xd "loop0"
  sdiv r1 r1 r2

  cmp r1 r0
  bcond 0xc "rem"

  addLabel "code" "loop1"
  rdivRoutine

  mov r0 r2
  addi r0 r0 48
  stripre r1 sp (-16)
  stripre r3 sp (-16)
  printChar
  ldri r3 sp 16
  ldri r1 sp 16

  mov r0 r3
  movzx r2 10
  sdiv r1 r1 r2

  movzx r4 1
  cmp r1 r4
  bcond 0xc "loop1"

  addLabel "code" "rem"
  addi r0 r0 48
  printChar

  printRoutine "\n"

  ret lr

printRoutine :: String -> RelocatableWriter ()
printRoutine str = do
  (lab, len) <- addString str
  movzx r0 1
  adr r1 lab
  movzx r2 (fromIntegral len)
  movzx r8 64
  svc 0

-- r0 dividend
-- r1 divisor
-- r2 quotient
-- r3 remainder
rdivRoutine :: RelocatableWriter ()
rdivRoutine = do
  sdiv r2 r0 r1
  msub r3 r2 r1 r0

-- r0 ascii char
printChar :: RelocatableWriter ()
printChar = do
  adr r1 "printNumber_buf"
  stri r0 r1 0
  movzx r0 1
  movzx r2 1
  movzx r8 64
  svc 0

printBinary :: RelocatableWriter ()
printBinary = do
  addLabel "code" "printBinary"

  movzx r1 64

  addLabel "code" "binaryLoop"

  tbz r0 63 "p"

  stripre r0 sp (-16)
  stripre r1 sp (-16)
  movzx r0 49
  printChar
  ldri r1 sp 16
  ldri r0 sp 16

  b "c"

  addLabel "code" "p"

  stripre r0 sp (-16)
  stripre r1 sp (-16)
  movzx r0 48
  printChar
  ldri r1 sp 16
  ldri r0 sp 16

  addLabel "code" "c"

  lsli r0 r0 1 -- left shift
  subi r1 r1 1
  movzx r2 0
  cmp r1 r2
  bcond 0x1 "binaryLoop"

  printRoutine "\n"

  ret lr
