module LyGen_amd64 (lysseProgramAmd64) where

import Code_amd64 (label)
import Macros_amd64 (exitMacroAmd64, printMacro)
import Relocation (RelocatableWriter)
import STree (LAStatement)

lysseProgramAmd64 :: [LAStatement] -> RelocatableWriter ()
lysseProgramAmd64 _ = do
  label "_start"
  printMacro "hello, world\n"

  exitMacroAmd64 0