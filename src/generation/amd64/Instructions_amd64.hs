{-# LANGUAGE LambdaCase #-}

module Instructions_amd64 () where

import Data.Binary (Put, Word64, Word8, putWord8)
import Data.Binary.Put (putWord16be, putWord16le)
import Data.Bits (shift, (.|.))
import Relocation (Relocatable (Relocatable), RelocatableWriter, RelocationTable, addRelocatable)

addRelocatableInstruction :: (RelocationTable -> Put) -> Word64 -> Word64 -> RelocatableWriter ()
addRelocatableInstruction fn s a = addRelocatable "code" (Relocatable fn s a)

addInstruction :: Put -> Word64 -> Word64 -> RelocatableWriter ()
addInstruction p = addRelocatableInstruction (const p)

nop :: RelocatableWriter ()
nop = addInstruction (putWord16le 0x90) 1 1

syscall :: RelocatableWriter ()
syscall = addInstruction (putWord16be 0x0f05) 2 1

modrm :: Mode -> Register -> Register -> Put
modrm mode reg rm = putWord8 (shift (encodeMode mode) 6 .|. shift (encodeRegister reg) 3 .|. encodeRegister reg)

data Mode = RegisterIndirect | OneDisplacement | FourDisplacement | RegisterAddressing

encodeMode :: Mode -> Word8
encodeMode = \case
  RegisterIndirect -> 0
  OneDisplacement -> 1
  FourDisplacement -> 2
  RegisterAddressing -> 3

data Register = RA | RC | RD | RB | RSP | RBP | RSI | RDI

encodeRegister :: Register -> Word8
encodeRegister = \case
  RA -> 0
  RC -> 1
  RD -> 2
  RB -> 3
  RSP -> 4
  RBP -> 5
  RSI -> 6
  RDI -> 7
