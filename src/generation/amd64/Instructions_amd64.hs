{-# LANGUAGE LambdaCase #-}

module Instructions_amd64 (nop, indirectStore, movImm, addImm32, syscall, leaRipRel, Register (..), call, ret, push, pop) where

import Code_amd64 (createLabel, label)
import Data.Binary (Put, Word32, Word64, Word8, putWord8)
import Data.Binary.Put (putInt32le, putWord16be, putWord16le, putWord32le)
import Data.Bits (shift, (.|.))
import Data.Int (Int32)
import Relocation (Relocatable (Relocatable), RelocatableWriter, RelocationTable, addRelocatable, getRelativeAddress)

getRelative :: RelocationTable -> String -> String -> Word32
getRelative rt to from = fromIntegral (getRelativeAddress rt to from)

addRelocatableInstruction :: (RelocationTable -> Put) -> Word64 -> Word64 -> RelocatableWriter ()
addRelocatableInstruction fn s a = addRelocatable "code" (Relocatable fn s a)

addInstruction :: Put -> Word64 -> Word64 -> RelocatableWriter ()
addInstruction p = addRelocatableInstruction (const p)

nop :: RelocatableWriter ()
nop = addInstruction (putWord16le 0x90) 1 1

movImm :: Register -> Word32 -> RelocatableWriter ()
movImm r imm =
  addInstruction
    ( do
        putWord8 (0xb8 .|. encodeRegister r)
        putWord32le imm
    )
    5
    1

push :: Register -> RelocatableWriter ()
push r =
  addInstruction
    (putWord8 (0x50 .|. encodeRegister r))
    1
    1

pop :: Register -> RelocatableWriter ()
pop r =
  addInstruction
    (putWord8 (0x58 .|. encodeRegister r))
    1
    1

addImm32 :: Register -> Int32 -> RelocatableWriter ()
addImm32 rd imm =
  addInstruction
    ( do
        putWord8 0x48
        putWord8 0x81
        modrm RegisterAddressing RA rd
        putInt32le imm
    )
    5
    1

indirectStore :: Register -> Register -> RelocatableWriter ()
indirectStore rd rs =
  addInstruction
    ( do
        putWord8 0x48
        putWord8 0x89
        modrm RegisterIndirect rs rd
    )
    3
    1

syscall :: RelocatableWriter ()
syscall = addInstruction (putWord16be 0x0f05) 2 1

call :: String -> RelocatableWriter ()
call dest = do
  rel <- createLabel
  addRelocatableInstruction
    ( \rt ->
        do
          putWord8 0xE8
          putWord32le $ getRelative rt dest rel
    )
    5
    1
  label rel

ret :: RelocatableWriter ()
ret = addInstruction (putWord8 0xc3) 1 1

leaRipRel :: Register -> String -> RelocatableWriter ()
leaRipRel rd dest = do
  rel <- createLabel
  addRelocatableInstruction
    ( \rt -> do
        putWord8 0x48
        putWord8 0x8d

        modrm RegisterIndirect rd RBP
        putWord32le $ getRelative rt dest rel
    )
    7
    1
  label rel

modrm :: Mode -> Register -> Register -> Put
modrm mode reg rm = putWord8 (shift (encodeMode mode) 6 .|. shift (encodeRegister reg) 3 .|. encodeRegister rm)

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
