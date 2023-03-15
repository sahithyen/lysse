module Executable (generate, RelocationTable) where

import Data.Binary (Word16, Word32)
import Data.Binary.Put (PutM, putWord32le)
import DataWriter (offsetPutData)
import Elf (ELFHeaderParameter (ELFHeaderParameter), ELFProgramHeaderParameter (ELFProgramHeaderParameter), elfHeader, elfProgramHeader)
import Instructions (movzw, movzx, movzxLabel, r0, r1, r2, r8, svc)
import Relocation (RelocationTable, joinRelocationTable, offsetRelocations)
import RoutineWriter (RoutineWriter, addString, assembleRoutine, dataTable, programSize, relocate, relocationTable)

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
  movzw r8 93
  svc 0

printRoutine :: String -> RoutineWriter ()
printRoutine textLabel = do
  movzx r0 0
  movzxLabel r1 textLabel
  movzx r2 14
  movzx r8 64 -- 4 - write syscall
  svc 0

mainRoutine :: RoutineWriter ()
mainRoutine = do
  addString "hello" "hello, world!\n"
  printRoutine "hello"
  exitRoutine 0