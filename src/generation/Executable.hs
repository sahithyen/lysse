module Executable (generate, RelocationTable) where

import Data.Binary (Word32, Word64, Word8, putWord8)
import Data.Binary.Put (PutM, putLazyByteString, putWord32le, putWord64le)
import Data.ByteString.Lazy as BS (length)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Elf (ELFHeaderParameter (ELFHeaderParameter), ELFProgramHeaderParameter (ELFProgramHeaderParameter), elfHeader, elfProgramHeader)
import Instructions (add, addi, adr, bcond, bl, cmp, ldri, ldrlx, lr, madd, mov, movzx, msub, r0, r1, r2, r3, r4, r5, r6, r8, ret, sdiv, sp, stri, stripre, svc, wzr)
import Relocation (Relocatable (..), RelocatableWriter, RelocationTable, SegmentType (Data, Exec), addLabel, addLabeledRelocatable, addRelocatable, addSegment, executeRelocatableWriter, getUniqueLabel, resolveLabel)

addWord :: Word32 -> Relocatable
addWord v = Relocatable (const $ putWord32le v) 4 4

addDWord :: Word64 -> Relocatable
addDWord v = Relocatable (const $ putWord64le v) 8 8

addByte :: Word8 -> Relocatable
addByte v = Relocatable (const $ putWord8 v) 1 1

writeLabel :: String -> Relocatable
writeLabel l = Relocatable (\rt -> putWord64le (resolveLabel rt l)) 8 8

addString :: String -> RelocatableWriter (String, Word64)
addString s = do
  l <- getUniqueLabel
  addLabeledRelocatable "data" l $ Relocatable (const $ putLazyByteString bs) bsl 8
  return (l, bsl)
  where
    bs = encodeUtf8 . pack $ s
    bsl = fromIntegral $ BS.length bs

-- r0 exit code
exitRoutine :: RelocatableWriter ()
exitRoutine = do
  movzx r8 93
  svc 0

printRoutine :: String -> RelocatableWriter ()
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
rdivRoutine :: RelocatableWriter ()
rdivRoutine = do
  sdiv r2 r0 r1
  msub r3 r2 r1 r0

-- r0 ascii char
printChar :: RelocatableWriter ()
printChar = do
  adr r1 "printNumber_buf"
  stri r0 r1 0
  movzx r0 1
  movzx r2 1
  movzx r8 64
  svc 0

routine :: RelocatableWriter ()
routine = do
  addLabel "code" "_start"

  movzx r0 0
  bl "printNumber"

  movzx r0 1
  bl "printNumber"

  movzx r0 2
  bl "printNumber"

  movzx r0 3
  bl "printNumber"

  movzx r0 4
  bl "printNumber"

  movzx r0 10
  bl "printNumber"

  movzx r0 11
  bl "printNumber"

  movzx r0 100
  bl "printNumber"

  movzx r0 101
  bl "printNumber"

  movzx r0 0
  exitRoutine

  addLabel "code" "printNumber"
  addLabeledRelocatable "data" "printNumber_buf" $ addByte 0

  movzx r1 10
  movzx r2 10
  addLabel "code" "loop0"
  madd r1 r1 r2 wzr
  cmp r1 r0
  bcond 0xd "loop0"
  sdiv r1 r1 r2

  cmp r1 r0
  bcond 0xc "rem"

  addLabel "code" "loop1"
  rdivRoutine

  mov r0 r2
  addi r0 r0 48
  stripre r1 sp (-16)
  stripre r3 sp (-16)
  printChar
  ldri r3 sp 16
  ldri r1 sp 16

  mov r0 r3
  movzx r2 10
  sdiv r1 r1 r2

  movzx r4 1
  cmp r1 r4
  bcond 0xc "loop1"

  addLabel "code" "rem"
  addi r0 r0 48
  printChar

  printRoutine "\n"

  ret lr

defineSegments :: Word64 -> Word64 -> [(String, SegmentType)] -> RelocatableWriter ()
defineSegments _ _ [] = pure ()
defineSegments o a ((sname, st) : ss) = do
  addSegment sname o a st
  defineSegments 0 a ss

generate :: Data.Binary.Put.PutM ()
generate = do
  elfHeader $ ELFHeaderParameter executionEntryAddress (fromIntegral programHeadersCount)
  mapM_ elfProgramHeader programParameters
  putCode rt
  where
    segments = [("code", Exec), ("data", Data)]
    programHeadersCount = fromIntegral $ Prelude.length segments
    headerOffset = 0x40 + 0x38 * programHeadersCount
    segmentAlignment = 4096
    (putCode, rt, st) = executeRelocatableWriter $ sequence_ [defineSegments headerOffset segmentAlignment segments, routine]
    programParameters = fmap (\(ssize, soff, stype) -> ELFProgramHeaderParameter soff soff ssize ssize segmentAlignment stype) st
    executionEntryAddress = resolveLabel rt "_start"
