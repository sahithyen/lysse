module LyGen (lysseProgram) where

import Code (stacked)
import Data (addDWord, addLabeledDWord)
import Data.Binary (Word32)
import Instructions
  ( add,
    adr,
    bl,
    ldrlx,
    madd,
    r0,
    r1,
    sdiv,
    stri,
    sub,
    wzr,
  )
import Macros (exitMacro, printMacro)
import Relocation (RelocatableWriter, addLabel)
import Routines (routines)
import STree (LAExpression (..), LAIdentifier (LAIdentifier), LAStatement (..), getIdentifiers)

lysseProgram :: [LAStatement] -> RelocatableWriter ()
lysseProgram stmts = do
  routines
  addLabel "code" "_start"

  allocator stmts
  statementsW stmts

  exitMacro 0

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
statementW (LAInput (LAIdentifier _)) = do
  error "input not implemented"
statementW (LAOutput (LAIdentifier ident)) = do
  printMacro $ ident ++ " = "
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
