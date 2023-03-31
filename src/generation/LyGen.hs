{-# LANGUAGE LambdaCase #-}

module LyGen (lysseProgram) where

import Architecture (Architecture (..))
import LyGen_aarch64 (lysseProgramAarch64)
import LyGen_amd64 (lysseProgramAmd64)
import Relocation (RelocatableWriter)
import STree (LAStatement (..))

lysseProgram :: Architecture -> [LAStatement] -> RelocatableWriter ()
lysseProgram = \case
  Aarch64 -> lysseProgramAarch64
  Amd64 -> lysseProgramAmd64
