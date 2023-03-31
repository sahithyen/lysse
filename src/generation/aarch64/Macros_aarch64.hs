module Macros_aarch64 (exitMacro, printMacro) where

import Data (addString)
import Data.Binary (Word16)
import Instructions_aarch64
  ( adr,
    movzx,
    r0,
    r1,
    r2,
    r8,
    svc,
  )
import Relocation (RelocatableWriter)

exitMacro :: Word16 -> RelocatableWriter ()
exitMacro exitCode = do
  movzx r0 exitCode
  movzx r8 93
  svc 0

printMacro :: String -> RelocatableWriter ()
printMacro str = do
  (lab, len) <- addString str
  movzx r0 1
  adr r1 lab
  movzx r2 (fromIntegral len)
  movzx r8 64
  svc 0