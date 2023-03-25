{-# LANGUAGE NumericUnderscores #-}

module Elf (generateElf) where

import Data.Binary (Word16, Word64, Word8)
import Data.Binary.Put (Put, putWord16le, putWord32be, putWord32le, putWord64le, putWord8)
import Relocation
  ( RelocatableWriter,
    SegmentType (..),
    addSegment,
    executeRelocatableWriter,
    resolveLabel,
  )

data ELFHeaderParameter = ELFHeaderParameter
  { executionEntryAddress :: Word64,
    programHeadersCount :: Word16
  }

pad :: (Num a, Eq a) => Word8 -> a -> Put
pad _ 0 = pure ()
pad v n = do
  pad v (n - 1)
  putWord8 v

elfHeader :: ELFHeaderParameter -> Put
elfHeader parameters = do
  -- e_ident (Magic)
  putWord32be 0x7f_45_4C_46 -- 0x7f, ELF
  putWord8 0x02 -- 64 bit
  putWord8 0x01 -- Little endian
  putWord8 0x01 -- Version 1
  pad 0x00 (9 :: Int)

  -- e_type
  putWord16le 0x03 -- Executable

  -- e_machine
  putWord16le 0xB7 -- aarch64

  -- e_version
  putWord32le 0x01

  -- e_entry
  putWord64le (executionEntryAddress parameters)

  -- e_phoff
  putWord64le 0x40

  -- e_shoff
  putWord64le 0

  pad 0x00 (4 :: Int)

  -- e_ehsize
  putWord16le 0x40

  -- e_phentsize
  putWord16le 0x38

  -- e_phnum
  putWord16le (programHeadersCount parameters)

  -- e_shentsize
  putWord16le 0

  -- e_shnum
  putWord16le 0

  -- e_shstrndx
  putWord16le 0

data ELFProgramHeaderParameter = ELFProgramHeaderParameter
  { offset :: Word64,
    address :: Word64,
    fileSize :: Word64,
    memSize :: Word64,
    alignment :: Word64,
    segmentType :: SegmentType
  }

elfProgramHeader :: ELFProgramHeaderParameter -> Put
elfProgramHeader parameters = do
  -- p_type
  putWord32le 0x01

  -- p_flags
  -- PF_X 0x1
  -- PF_W 0x2
  -- PF_R 0x4
  let perm = case segmentType parameters of
        ROData -> 0x4
        Exec -> 0x5
        Data -> 0x6
  putWord32le perm

  -- p_offset
  putWord64le $ offset parameters

  -- p_vaddr
  putWord64le $ address parameters

  -- p_paddr
  putWord64le $ address parameters

  -- p_filesz
  putWord64le $ fileSize parameters

  -- p_memsz
  putWord64le $ memSize parameters

  -- p_align
  putWord64le $ alignment parameters

defineSegments :: Word64 -> Word64 -> [(String, SegmentType)] -> RelocatableWriter ()
defineSegments _ _ [] = pure ()
defineSegments o a ((sname, st) : ss) = do
  addSegment sname o a st
  defineSegments 0 a ss

generateElf :: RelocatableWriter () -> Put
generateElf routine = do
  elfHeader $ ELFHeaderParameter entry (fromIntegral segmentCount)
  mapM_ elfProgramHeader programParameters
  putCode rt
  where
    segments = [("code", Exec), ("data", Data)]
    segmentCount = fromIntegral $ Prelude.length segments
    headerOffset = 0x40 + 0x38 * segmentCount
    segmentAlignment = 4_096
    (putCode, rt, st) = executeRelocatableWriter $ sequence_ [defineSegments headerOffset segmentAlignment segments, routine]
    programParameters = fmap (\(ssize, soff, stype) -> ELFProgramHeaderParameter soff soff ssize ssize segmentAlignment stype) st
    entry = resolveLabel rt "_start"
