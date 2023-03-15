{-# LANGUAGE NumericUnderscores #-}

module Elf (ELFHeaderParameter (..), elfHeader, ELFProgramHeaderParameter (..), elfProgramHeader) where

import Data.Binary (Word16, Word64, Word8)
import Data.Binary.Put (PutM, putWord16le, putWord32be, putWord32le, putWord64le, putWord8)

data ELFHeaderParameter = ELFHeaderParameter
  { executionEntryAddress :: Word64,
    programHeadersEntrySize :: Word16,
    programHeadersCount :: Word16
  }

pad :: (Num a, Eq a) => Word8 -> a -> PutM ()
pad _ 0 = pure ()
pad v n = do
  pad v (n - 1)
  putWord8 v

elfHeader :: ELFHeaderParameter -> PutM ()
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
  putWord16le (programHeadersEntrySize parameters)

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
    virtualAddress :: Word64,
    physicalAddress :: Word64,
    fileSize :: Word64,
    memSize :: Word64
  }

elfProgramHeader :: ELFProgramHeaderParameter -> PutM ()
elfProgramHeader parameters = do
  -- p_type
  putWord32le 0x01

  -- p_flags
  putWord32le 0x05

  -- p_offset
  putWord64le (offset parameters)

  -- p_vaddr
  putWord64le (virtualAddress parameters)

  -- p_paddr
  putWord64le (physicalAddress parameters)

  -- p_filesz
  putWord64le (fileSize parameters)

  -- p_memsz
  putWord64le (memSize parameters)

  -- p_align
  putWord64le 0x10_000