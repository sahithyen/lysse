module Executable (generate, RelocationTable) where

import Data.Binary (Word32, Word64, Word8, putWord8)
import Data.Binary.Put (PutM, putWord32le, putWord64le)
import Elf (ELFHeaderParameter (ELFHeaderParameter), ELFProgramHeaderParameter (ELFProgramHeaderParameter), elfHeader, elfProgramHeader)
import Relocation (Relocatable (..), RelocatableWriter, RelocationTable, SegmentType (Data, Exec), addLabel, addLabeledRelocatable, addRelocatable, addSegment, executeRelocatableWriter, offsetRelocations, resolveLabel)

addWord :: Word32 -> Relocatable
addWord v = Relocatable (const $ putWord32le v) 4 4

addByte :: Word8 -> Relocatable
addByte v = Relocatable (const $ putWord8 v) 1 1

writeLabel :: String -> Relocatable
writeLabel l = Relocatable (\rt -> putWord64le (resolveLabel rt l)) 8 8

routine :: RelocatableWriter ()
routine = do
  addLabel "code" "_start"
  addRelocatable "code" (addWord 0xfffefdfc)

  addLabel "data" "s"
  addLabeledRelocatable "data" "d" $ addWord 0xcafecafe

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
    programHeadersCount = fromIntegral $ length segments
    headerOffset = 0x40 + 0x38 * programHeadersCount
    segmentAlignment = 4096
    (putCode, rt, st) = executeRelocatableWriter $ sequence_ [defineSegments headerOffset segmentAlignment segments, routine]
    programParameters = fmap (\(ssize, soff, stype) -> ELFProgramHeaderParameter soff soff ssize ssize segmentAlignment stype) st
    executionEntryAddress = resolveLabel rt "_start"
