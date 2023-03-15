{-# LANGUAGE NumericUnderscores #-}

module Executable (generate, RelocationTable) where

import Data.Binary (Word16, Word32, Word8)
import Data.Binary.Put (PutM, putWord32le)
import Data.Bits (shift, (.&.), (.|.))
import DataWriter (offsetPutData)
import Elf (ELFHeaderParameter (ELFHeaderParameter), ELFProgramHeaderParameter (ELFProgramHeaderParameter), elfHeader, elfProgramHeader)
import Relocation (RelocationTable, joinRelocationTable, offsetRelocations)
import RoutineWriter (RoutineWriter, addInstruction, addLabel, addRelocatableInstruction, addUniqueLabel, addWord, assembleRoutine, dataTable, getRelativeAddress, programSize, relocate, relocationTable)

movzw :: Word8 -> Word16 -> RoutineWriter ()
movzw reg imm = addInstruction $ (0x52_80_00_00 :: Word32) .|. shift (fromIntegral imm :: Word32) 5 .|. (fromIntegral reg :: Word32)

movzx :: Word8 -> Word16 -> RoutineWriter ()
movzx reg imm = addInstruction $ (0xd2_80_00_00 :: Word32) .|. shift (fromIntegral imm :: Word32) 5 .|. (fromIntegral reg :: Word32)

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

r8 :: Word8
r8 = 8

putRoutine :: [Word32] -> PutM ()
putRoutine [] = pure ()
putRoutine (x : xs) = do
  putWord32le x
  putRoutine xs

generate :: Data.Binary.Put.PutM ()
generate = do
  elfHeader (ELFHeaderParameter executionEntryAddress programHeadersEntrySize programHeadersCount)
  elfProgramHeader (ELFProgramHeaderParameter programFileOffset loadAddress loadAddress programFileSize programFileSize)
  putRoutine program
  putData
  where
    elfHeaderSize = 0x40
    programHeadersEntrySize = 0x38
    programHeadersCount = 1
    programHeadersSize = programHeadersEntrySize * programHeadersCount
    headerSize = fromIntegral (elfHeaderSize + programHeadersSize)
    (routineState, relocatableProgram) = assembleRoutine mainRoutine
    programFileSize = headerSize + programSize routineState
    programRt = offsetRelocations headerSize (relocationTable routineState)
    (putData, dataRt) = offsetPutData programFileSize (dataTable routineState)
    fullRt = joinRelocationTable programRt dataRt
    program = relocate relocatableProgram fullRt
    programFileOffset = 0 -- Offset in file needs to be a multiple of the page size (page size in arm: 4096 bytes)
    loadAddress = 0
    executionEntryAddress = loadAddress + headerSize

exitRoutine :: Word16 -> RoutineWriter ()
exitRoutine exitCode = do
  movzx r0 exitCode
  ldrlx "mol"
  movzw r8 93
  svc 0

mainRoutine :: RoutineWriter ()
mainRoutine = do
  addWord "mol" 42
  b "case2"
  addLabel "case1"
  exitRoutine 21
  addLabel "case2"
  exitRoutine 84