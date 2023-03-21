{-# LANGUAGE NumericUnderscores #-}

module Instructions (cmp, add, addi, sdiv, madd, msub, movzw, movzx, mov, ldrlx, stri, ldri, stripre, svc, adr, lr, b, bl, ret, bcond, r0, r1, r2, r3, r4, r5, r6, r8, wzr, sp) where

import Data.Binary (Word16, Word32)
import Data.Binary.Put (putWord32le)
import Data.Bits (shift, (.&.), (.|.))
import Data.Int (Int16)
import Relocation (Relocatable (Relocatable), RelocatableWriter, RelocationTable, addRelocatable, addUniqueLabel, getRelativeAddress)

getByteRelative :: RelocationTable -> String -> String -> Word32
getByteRelative rt label relLabel = fromIntegral (getRelativeAddress rt label relLabel)

getWordRelative :: RelocationTable -> String -> String -> Word32
getWordRelative rt label relLabel = getByteRelative rt label relLabel `div` 4

addRelocatableInstruction :: (RelocationTable -> Word32) -> RelocatableWriter ()
addRelocatableInstruction fn = do
  addRelocatable "code" (Relocatable (putWord32le . fn) 4 4)
  return ()

addInstruction :: Word32 -> RelocatableWriter ()
addInstruction i = addRelocatableInstruction $ const i

movzw :: Word32 -> Word16 -> RelocatableWriter ()
movzw reg imm = addInstruction $ (0x52_80_00_00 :: Word32) .|. shift (fromIntegral imm :: Word32) 5 .|. reg

movzx :: Word32 -> Word16 -> RelocatableWriter ()
movzx reg imm = addInstruction $ (0xd2_80_00_00 :: Word32) .|. shift (fromIntegral imm :: Word32) 5 .|. reg

mov :: Word32 -> Word32 -> RelocatableWriter ()
mov d m = addInstruction $ 0xaa_00_03_e0 .|. shift m 16 .|. d

ldrlx :: Word32 -> String -> RelocatableWriter ()
ldrlx reg label = do
  relLabel <- addUniqueLabel "code"
  addRelocatableInstruction $ \rt ->
    0x58_00_00_00 .|. (shift (getWordRelative rt label relLabel) 5 .&. 0x00_FF_FF_E0) .|. reg

adr :: Word32 -> String -> RelocatableWriter ()
adr reg label = do
  relLabel <- addUniqueLabel "code"
  addRelocatableInstruction $ \rt ->
    (shift (getByteRelative rt label relLabel) 29 .&. 0x60_00_00_00) .|. 0x10_00_00_00 .|. (shift (getByteRelative rt label relLabel) 3 .&. 0x00_FF_FF_E0) .|. reg

stri :: Word32 -> Word32 -> Word32 -> RelocatableWriter ()
stri t n imm = addInstruction $ 0xf9_00_00_00 .|. shift imm 12 .|. shift n 5 .|. t

stripre :: Word32 -> Word32 -> Int16 -> RelocatableWriter ()
stripre t n imm = addInstruction $ 0xf8_00_0c_00 .|. shift (fromIntegral imm .&. 0x1ff) 12 .|. shift n 5 .|. t

ldri :: Word32 -> Word32 -> Int16 -> RelocatableWriter ()
ldri t n imm = addInstruction $ 0xf8_40_04_00 .|. shift (fromIntegral imm .&. 0x1ff) 12 .|. shift n 5 .|. t

svc :: Word16 -> RelocatableWriter ()
svc imm = addInstruction $ (0xd4_00_00_01 :: Word32) .|. shift (fromIntegral imm :: Word32) 5

b :: String -> RelocatableWriter ()
b label = do
  relLabel <- addUniqueLabel "code"
  addRelocatableInstruction $ \rt ->
    0x14_00_00_00 .|. (getWordRelative rt label relLabel .&. 0x03_FF_FF_FF)

bl :: String -> RelocatableWriter ()
bl label = do
  relLabel <- addUniqueLabel "code"
  addRelocatableInstruction $ \rt ->
    0x94_00_00_00 .|. (getWordRelative rt label relLabel .&. 0x03_FF_FF_FF)

ret :: Word32 -> RelocatableWriter ()
ret rn = addInstruction $ 0xd6_5f_00_00 .|. shift rn 5

addi :: Word32 -> Word32 -> Word32 -> RelocatableWriter ()
addi rd rn imm = addInstruction $ 0x91_00_00_00 .|. shift imm 10 .|. shift rn 5 .|. rd

add :: Word32 -> Word32 -> Word32 -> RelocatableWriter ()
add ra rb rd = addInstruction $ 0xab_00_00_00 .|. shift ra 16 .|. shift rb 5 .|. rd

sdiv :: Word32 -> Word32 -> Word32 -> RelocatableWriter ()
sdiv d n m = addInstruction $ 0x9a_c0_0c_00 .|. shift m 16 .|. shift n 5 .|. d

msub :: Word32 -> Word32 -> Word32 -> Word32 -> RelocatableWriter ()
msub d n m a = addInstruction $ 0x9b_00_80_00 .|. shift m 16 .|. shift a 10 .|. shift n 5 .|. d

madd :: Word32 -> Word32 -> Word32 -> Word32 -> RelocatableWriter ()
madd d n m a = addInstruction $ 0x9b_00_00_00 .|. shift m 16 .|. shift a 10 .|. shift n 5 .|. d

bcond :: Word32 -> String -> RelocatableWriter ()
bcond f label = do
  relLabel <- addUniqueLabel "code"
  addRelocatableInstruction $ \rt ->
    0x54_00_00_00 .|. shift (getWordRelative rt label relLabel .&. 0x00_07_FF_FF) 5 .|. f

cmp :: Word32 -> Word32 -> RelocatableWriter ()
cmp n m = addInstruction $ 0xeb_00_00_1f .|. shift m 16 .|. shift n 5

r0 :: Word32
r0 = 0

r1 :: Word32
r1 = 1

r2 :: Word32
r2 = 2

r3 :: Word32
r3 = 3

r4 :: Word32
r4 = 4

r5 :: Word32
r5 = 5

r6 :: Word32
r6 = 6

r8 :: Word32
r8 = 8

lr :: Word32
lr = 0x1e

wzr :: Word32
wzr = 0x1f

sp :: Word32
sp = 0x1f