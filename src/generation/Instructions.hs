{-# LANGUAGE NumericUnderscores #-}

module Instructions (cmp, add, sub, neg, addi, subi, sdiv, madd, msub, lsri, lsli, movzw, movzx, mov, ldrlx, stri, ldri, stripre, svc, adr, lr, b, bl, tbz, ret, bcond, r0, r1, r2, r3, r4, r5, r6, r8, wzr, sp) where

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
add rd ra rb = addInstruction $ 0xab_00_00_00 .|. shift ra 16 .|. shift rb 5 .|. rd

subi :: Word32 -> Word32 -> Word32 -> RelocatableWriter ()
subi rd rn imm = addInstruction $ 0xd1_00_00_00 .|. shift imm 10 .|. shift rn 5 .|. rd

sub :: Word32 -> Word32 -> Word32 -> RelocatableWriter ()
sub rd rb ra = addInstruction $ 0xcb_00_00_00 .|. shift ra 16 .|. shift rb 5 .|. rd

sdiv :: Word32 -> Word32 -> Word32 -> RelocatableWriter ()
sdiv d n m = addInstruction $ 0x9a_c0_0c_00 .|. shift m 16 .|. shift n 5 .|. d

msub :: Word32 -> Word32 -> Word32 -> Word32 -> RelocatableWriter ()
msub d n m a = addInstruction $ 0x9b_00_80_00 .|. shift m 16 .|. shift a 10 .|. shift n 5 .|. d

neg :: Word32 -> Word32 -> RelocatableWriter ()
neg d m = addInstruction $ 0xcb_00_03_e0 .|. shift m 16 .|. d

madd :: Word32 -> Word32 -> Word32 -> Word32 -> RelocatableWriter ()
madd d n m a = addInstruction $ 0x9b_00_00_00 .|. shift m 16 .|. shift a 10 .|. shift n 5 .|. d

-- https://developer.arm.com/documentation/den0024/a/The-A64-instruction-set/Data-processing-instructions/Conditional-instructions
bcond :: Word32 -> String -> RelocatableWriter ()
bcond f label = do
  relLabel <- addUniqueLabel "code"
  addRelocatableInstruction $ \rt ->
    0x54_00_00_00 .|. shift (getWordRelative rt label relLabel .&. 0x00_07_FF_FF) 5 .|. f

tbz :: Word32 -> Word32 -> String -> RelocatableWriter ()
tbz t bit label = do
  relLabel <- addUniqueLabel "code"
  addRelocatableInstruction $ \rt ->
    0x36_00_00_00 .|. shift (shift bit (-4) .&. 1) 31 .|. shift (bit .&. 0x1f) 19 .|. shift (getWordRelative rt label relLabel .&. 0x00_00_1F_FF) 5 .|. t

cmp :: Word32 -> Word32 -> RelocatableWriter ()
cmp n m = addInstruction $ 0xeb_00_00_1f .|. shift m 16 .|. shift n 5

lsri :: Word32 -> Word32 -> Word32 -> RelocatableWriter ()
lsri rd rn imm = addInstruction $ 0xd3_40_fc_00 .|. shift (imm .&. 0x3f) 16 .|. shift rn 5 .|. rd

lsli :: Word32 -> Word32 -> Word32 -> RelocatableWriter ()
lsli rd rn imm = addInstruction $ 0xd3_40_00_00 .|. shift (64 - (imm .&. 0x3f)) 16 .|. shift (63 - (imm .&. 0x3f)) 10 .|. shift rn 5 .|. rd

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