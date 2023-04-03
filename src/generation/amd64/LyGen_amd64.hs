module LyGen_amd64 (lysseProgramAmd64) where

import Code_amd64 (label)
import Macros_amd64 (exitMacroAmd64, printMacro)
import Relocation (RelocatableWriter)
import Routines_amd64 (printChar)
import STree (LAStatement)

lysseProgramAmd64 :: [LAStatement] -> RelocatableWriter ()
lysseProgramAmd64 _ = do
  label "_start"
  printMacro "hello, world\n"

  printChar

  exitMacroAmd64 0