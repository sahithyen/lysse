module Executable (generate, RelocationTable) where

import Data.Binary (Word64)
import Data.Binary.Put (PutM)
import Elf (ELFHeaderParameter (ELFHeaderParameter), ELFProgramHeaderParameter (ELFProgramHeaderParameter), elfHeader, elfProgramHeader)
import Relocation
  ( RelocatableWriter,
    RelocationTable,
    SegmentType (Data, Exec),
    addSegment,
    executeRelocatableWriter,
    resolveLabel,
  )

defineSegments :: Word64 -> Word64 -> [(String, SegmentType)] -> RelocatableWriter ()
defineSegments _ _ [] = pure ()
defineSegments o a ((sname, st) : ss) = do
  addSegment sname o a st
  defineSegments 0 a ss

generate :: RelocatableWriter () -> Data.Binary.Put.PutM ()
generate routine = do
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
