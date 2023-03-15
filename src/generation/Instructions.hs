{-# LANGUAGE NumericUnderscores #-}

module Instructions (movzw, movzx, ldrlx, svc, b, r0, r1, r2, r8, movzxLabel) where

import Data.Binary (Word16, Word32, Word8)
import Data.Bits (shift, (.&.), (.|.))
import RoutineWriter (RoutineWriter, addInstruction, addRelocatableInstruction, addUniqueLabel, getLabel, getRelativeAddress)

movzw :: Word8 -> Word16 -> RoutineWriter ()
movzw reg imm = addInstruction $ (0x52_80_00_00 :: Word32) .|. shift (fromIntegral imm :: Word32) 5 .|. (fromIntegral reg :: Word32)

movzx :: Word8 -> Word16 -> RoutineWriter ()
movzx reg imm = addInstruction $ (0xd2_80_00_00 :: Word32) .|. shift (fromIntegral imm :: Word32) 5 .|. (fromIntegral reg :: Word32)

movzxLabel :: Word8 -> String -> RoutineWriter ()
movzxLabel reg l = addRelocatableInstruction $ \rt -> (0xd2_80_00_00 :: Word32) .|. shift (fromIntegral (getLabel rt l) :: Word32) 5 .|. (fromIntegral reg :: Word32)

ldrlx :: String -> RoutineWriter ()
ldrlx label = do
  relLabel <- addUniqueLabel
  addRelocatableInstruction $ \rt ->
    0x58_00_00_00 .|. (shift (fromIntegral (getRelativeAddress rt label relLabel `div` 4)) 5 .&. 0x00_FF_FF_E0)

svc :: Word16 -> RoutineWriter ()
svc imm = addInstruction $ (0xd4_00_00_01 :: Word32) .|. shift (fromIntegral imm :: Word32) 5

b :: String -> RoutineWriter ()
b label = do
  relLabel <- addUniqueLabel
  addRelocatableInstruction $ \rt ->
    0x14_00_00_00 .|. (fromIntegral (getRelativeAddress rt label relLabel `div` 4) .&. 0x03_FF_FF_FF)

r0 :: Word8
r0 = 0

r1 :: Word8
r1 = 1

r2 :: Word8
r2 = 2

r8 :: Word8
r8 = 8