{-# LANGUAGE NumericUnderscores #-}

module Instructions (movzw, movzx, ldrlx, svc, adr, b, r0, r1, r2, r8, movzxLabel) where

import Data.Binary (Word16, Word32, Word8)
import Data.Bits (shift, (.&.), (.|.))
import Relocation (RelocationTable)
import RoutineWriter (RoutineWriter, addInstruction, addRelocatableInstruction, addUniqueLabel, getLabel, getRelativeAddress)

getByteRelative :: RelocationTable -> String -> String -> Word32
getByteRelative rt label relLabel = fromIntegral (getRelativeAddress rt label relLabel)

getWordRelative :: RelocationTable -> String -> String -> Word32
getWordRelative rt label relLabel = getByteRelative rt label relLabel `div` 4

movzw :: Word8 -> Word16 -> RoutineWriter ()
movzw reg imm = addInstruction $ (0x52_80_00_00 :: Word32) .|. shift (fromIntegral imm :: Word32) 5 .|. (fromIntegral reg :: Word32)

movzx :: Word8 -> Word16 -> RoutineWriter ()
movzx reg imm = addInstruction $ (0xd2_80_00_00 :: Word32) .|. shift (fromIntegral imm :: Word32) 5 .|. (fromIntegral reg :: Word32)

movzxLabel :: Word8 -> String -> RoutineWriter ()
movzxLabel reg l = addRelocatableInstruction $ \rt -> (0xd2_80_00_00 :: Word32) .|. shift (fromIntegral (getLabel rt l) :: Word32) 5 .|. (fromIntegral reg :: Word32)

ldrlx :: Word8 -> String -> RoutineWriter ()
ldrlx reg label = do
  relLabel <- addUniqueLabel
  addRelocatableInstruction $ \rt ->
    0x58_00_00_00 .|. (shift (getWordRelative rt label relLabel) 5 .&. 0x00_FF_FF_E0) .|. (fromIntegral reg :: Word32)

adr :: Word8 -> String -> RoutineWriter ()
adr reg label = do
  relLabel <- addUniqueLabel
  addRelocatableInstruction $ \rt ->
    (shift (getByteRelative rt label relLabel) 29 .&. 0x60_00_00_00) .|. 0x10_00_00_00 .|. (shift (getByteRelative rt label relLabel) 3 .&. 0x00_FF_FF_E0) .|. (fromIntegral reg :: Word32)

svc :: Word16 -> RoutineWriter ()
svc imm = addInstruction $ (0xd4_00_00_01 :: Word32) .|. shift (fromIntegral imm :: Word32) 5

b :: String -> RoutineWriter ()
b label = do
  relLabel <- addUniqueLabel
  addRelocatableInstruction $ \rt ->
    0x14_00_00_00 .|. (getWordRelative rt label relLabel .&. 0x03_FF_FF_FF)

r0 :: Word8
r0 = 0

r1 :: Word8
r1 = 1

r2 :: Word8
r2 = 2

r8 :: Word8
r8 = 8