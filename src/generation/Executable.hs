module Executable (generate, RelocationTable) where

import Data.Binary (Word16, Word32)
import Data.Binary.Put (PutM, putWord32le)
import DataWriter (offsetPutData)
import Elf (ELFHeaderParameter (ELFHeaderParameter), ELFProgramHeaderParameter (ELFProgramHeaderParameter), elfHeader, elfProgramHeader)
import Instructions (adr, ldrlx, mov, movzw, movzx, msub, r0, r1, r2, r3, r8, sdiv, stri, svc)
import Relocation (RelocationTable, joinRelocationTable, offsetRelocations)
import RoutineWriter (RoutineWriter, addDWord, addString, assembleRoutine, dataTable, programSize, relocate, relocationTable)

putRoutine :: [Word32] -> PutM ()
putRoutine [] = pure ()
putRoutine (x : xs) = do
  putWord32le x
  putRoutine xs

generate :: Data.Binary.Put.PutM ()
generate = do
  elfHeader (ELFHeaderParameter executionEntryAddress programHeadersEntrySize programHeadersCount)
  elfProgramHeader (ELFProgramHeaderParameter programFileOffset loadAddress loadAddress fullSize fullSize)
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
    (putData, dataRt, dataSize) = offsetPutData programFileSize (dataTable routineState)
    fullRt = joinRelocationTable programRt dataRt
    fullSize = programFileSize + dataSize
    program = relocate relocatableProgram fullRt
    programFileOffset = 0 -- Offset in file needs to be a multiple of the page size (page size in arm: 4096 bytes)
    loadAddress = 0
    executionEntryAddress = loadAddress + headerSize

-- r0 exit code
exitRoutine :: RoutineWriter ()
exitRoutine = do
  movzw r8 93
  svc 0

printRoutine :: String -> RoutineWriter ()
printRoutine str = do
  (lab, len) <- addString str
  movzx r0 1
  adr r1 lab
  movzx r2 (fromIntegral len)
  movzx r8 64
  svc 0

-- r0 dividend
-- r1 divisor
-- r2 quotient
-- r3 remainder
rdivRoutine :: RoutineWriter ()
rdivRoutine = do
  sdiv r2 r0 r1
  msub r3 r2 r1 r0

mainRoutine :: RoutineWriter ()
mainRoutine = do
  addDWord "buf" 200

  -- Calculate 20 / 8
  movzx r0 20
  movzx r1 7
  rdivRoutine
  mov r0 r3
  -- 6

  adr r1 "buf"
  -- stri r3 r1 0

  ldrlx r0 "buf"

  exitRoutine