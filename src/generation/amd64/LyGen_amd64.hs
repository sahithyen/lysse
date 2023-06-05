module LyGen_amd64 (lysseProgramAmd64) where

import Code (label)
import Instructions_amd64 (Register (RDI), call, movImm)
import Macros_amd64 (exitMacroAmd64, printMacro)
import Relocation (RelocatableWriter)
import Routines_amd64 (routines)
import STree (LAStatement)

lysseProgramAmd64 :: [LAStatement] -> RelocatableWriter ()
lysseProgramAmd64 _ = do
  routines
  label "_start"

  movImm RDI 12
  call "printNumber"

  printMacro "\n"

  exitMacroAmd64 0