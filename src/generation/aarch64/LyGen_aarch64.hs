module LyGen_aarch64 (lysseProgramAarch64) where

import Allocator (allocator, getIdentifierLabel)
import Code_aarch64 (call, label, stacked)
import Data (addDWord)
import Data.Binary (Word32)
import Instructions_aarch64
  ( add,
    adr,
    ldrlx,
    lr,
    madd,
    r0,
    r1,
    sdiv,
    stri,
    sub,
    wzr,
  )
import Macros_aarch64 (exitMacro, printMacro)
import Relocation (RelocatableWriter)
import Routines_aarch64 (routines)
import STree (LAExpression (..), LAIdentifier (LAIdentifier), LAStatement (..))

lysseProgramAarch64 :: [LAStatement] -> RelocatableWriter ()
lysseProgramAarch64 stmts = do
  routines

  allocator stmts

  label "_start"
  statementsW stmts

  exitMacro 0

statementsW :: [LAStatement] -> RelocatableWriter ()
statementsW = mapM_ statementW

statementW :: LAStatement -> RelocatableWriter ()
statementW (LAInput (LAIdentifier ident)) = do
  printMacro $ "Enter value for " ++ ident ++ ": "
  call [lr] "readNumber"
  adr r1 $ getIdentifierLabel ident
  stri r0 r1 0
statementW (LAOutput (LAIdentifier ident)) = do
  printMacro $ ident ++ " = "
  ldrlx r0 $ getIdentifierLabel ident
  call [lr] "printNumber"
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
expressionW rd (LAMultiplication a_op b_op) =
  withSpareRegister rd $ \rb ->
    do
      expressionW rd a_op
      expressionW rb b_op
      madd rd rd rb wzr
expressionW rd (LADivision a_op b_op) =
  withSpareRegister rd $ \rb ->
    do
      expressionW rd a_op
      expressionW rb b_op
      sdiv rd rd rb
expressionW rd (LAReference (LAIdentifier ident)) = ldrlx rd $ getIdentifierLabel ident
expressionW rd (LAInteger a) =
  do
    l <- addDWord a
    ldrlx rd l

withSpareRegister :: Word32 -> (Word32 -> RelocatableWriter ()) -> RelocatableWriter ()
withSpareRegister r fn =
  if r < 8
    then fn $ r + 1
    else stacked [r - 1] $ fn (r - 1)
