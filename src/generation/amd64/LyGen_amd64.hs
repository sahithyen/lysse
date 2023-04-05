module LyGen_amd64 (lysseProgramAmd64) where

import Code_amd64 (label)
import Instructions_amd64 (call)
import Macros_amd64 (exitMacroAmd64, printMacro)
import Relocation (RelocatableWriter)
import Routines_amd64 (routines)
import STree (LAStatement)

lysseProgramAmd64 :: [LAStatement] -> RelocatableWriter ()
lysseProgramAmd64 _ = do
  routines
  label "_start"

  call "printDigit"

  exitMacroAmd64 0