{-# LANGUAGE NumericUnderscores #-}

module Instructions (add, sdiv, msub, movzw, movzx, mov, ldrlx, stri, svc, adr, b, r0, r1, r2, r3, r8) where

import Data.Binary (Word16, Word32)
import Data.Bits (shift, (.&.), (.|.))
import Relocation (RelocationTable)
import RoutineWriter (RoutineWriter, addInstruction, addRelocatableInstruction, addUniqueLabel, getRelativeAddress)

getByteRelative :: RelocationTable -> String -> String -> Word32
getByteRelative rt label relLabel = fromIntegral (getRelativeAddress rt label relLabel)

getWordRelative :: RelocationTable -> String -> String -> Word32
getWordRelative rt label relLabel = getByteRelative rt label relLabel `div` 4

movzw :: Word32 -> Word16 -> RoutineWriter ()
movzw reg imm = addInstruction $ (0x52_80_00_00 :: Word32) .|. shift (fromIntegral imm :: Word32) 5 .|. reg

movzx :: Word32 -> Word16 -> RoutineWriter ()
movzx reg imm = addInstruction $ (0xd2_80_00_00 :: Word32) .|. shift (fromIntegral imm :: Word32) 5 .|. reg

mov :: Word32 -> Word32 -> RoutineWriter ()
mov d m = addInstruction $ 0xaa_00_03_e0 .|. shift m 16 .|. d

ldrlx :: Word32 -> String -> RoutineWriter ()
ldrlx reg label = do
  relLabel <- addUniqueLabel
  addRelocatableInstruction $ \rt ->
    0x58_00_00_00 .|. (shift (getWordRelative rt label relLabel) 5 .&. 0x00_FF_FF_E0) .|. reg

adr :: Word32 -> String -> RoutineWriter ()
adr reg label = do
  relLabel <- addUniqueLabel
  addRelocatableInstruction $ \rt ->
    (shift (getByteRelative rt label relLabel) 29 .&. 0x60_00_00_00) .|. 0x10_00_00_00 .|. (shift (getByteRelative rt label relLabel) 3 .&. 0x00_FF_FF_E0) .|. reg

stri :: Word32 -> Word32 -> Word32 -> RoutineWriter ()
stri t n imm = addInstruction $ 0xf9_00_00_00 .|. shift imm 12 .|. shift n 5 .|. t

svc :: Word16 -> RoutineWriter ()
svc imm = addInstruction $ (0xd4_00_00_01 :: Word32) .|. shift (fromIntegral imm :: Word32) 5

b :: String -> RoutineWriter ()
b label = do
  relLabel <- addUniqueLabel
  addRelocatableInstruction $ \rt ->
    0x14_00_00_00 .|. (getWordRelative rt label relLabel .&. 0x03_FF_FF_FF)

add :: Word32 -> Word32 -> Word32 -> RoutineWriter ()
add ra rb rd = addInstruction $ 0xab_00_00_00 .|. shift ra 16 .|. shift rb 5 .|. rd

sdiv :: Word32 -> Word32 -> Word32 -> RoutineWriter ()
sdiv d n m = addInstruction $ 0x9a_c0_0c_00 .|. shift m 16 .|. shift n 5 .|. d

msub :: Word32 -> Word32 -> Word32 -> Word32 -> RoutineWriter ()
msub d n m a = addInstruction $ 0x9b_00_80_00 .|. shift m 16 .|. shift a 10 .|. shift n 5 .|. d

r0 :: Word32
r0 = 0

r1 :: Word32
r1 = 1

r2 :: Word32
r2 = 2

r3 :: Word32
r3 = 3

r8 :: Word32
r8 = 8