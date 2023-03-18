module Executable (generate, RelocationTable) where

import Data.Binary (Word32, Word64, Word8, putWord8)
import Data.Binary.Put (PutM, putLazyByteString, putWord32le, putWord64le)
import Data.ByteString.Lazy as BS (length)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Elf (ELFHeaderParameter (ELFHeaderParameter), ELFProgramHeaderParameter (ELFProgramHeaderParameter), elfHeader, elfProgramHeader)
import Instructions (movzx, r0, r8, svc)
import Relocation (Relocatable (..), RelocatableWriter, RelocationTable, SegmentType (Data, Exec), addLabel, addLabeledRelocatable, addSegment, executeRelocatableWriter, getUniqueLabel, resolveLabel)

addWord :: Word32 -> Relocatable
addWord v = Relocatable (const $ putWord32le v) 4 4

addByte :: Word8 -> Relocatable
addByte v = Relocatable (const $ putWord8 v) 1 1

writeLabel :: String -> Relocatable
writeLabel l = Relocatable (\rt -> putWord64le (resolveLabel rt l)) 8 8

addString :: String -> RelocatableWriter (String, Word64)
addString s = do
  l <- getUniqueLabel
  addLabeledRelocatable "data" l $ Relocatable (const $ putLazyByteString bs) bsl 1
  return (l, bsl)
  where
    bs = encodeUtf8 . pack $ s
    bsl = fromIntegral $ BS.length bs

routine :: RelocatableWriter ()
routine = do
  addLabel "code" "_start"
  movzx r0 0
  movzx r8 93
  svc 0

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
    programHeadersCount = fromIntegral $ Prelude.length segments
    headerOffset = 0x40 + 0x38 * programHeadersCount
    segmentAlignment = 4096
    (putCode, rt, st) = executeRelocatableWriter $ sequence_ [defineSegments headerOffset segmentAlignment segments, routine]
    programParameters = fmap (\(ssize, soff, stype) -> ELFProgramHeaderParameter soff soff ssize ssize segmentAlignment stype) st
    executionEntryAddress = resolveLabel rt "_start"
