{-# LANGUAGE NumericUnderscores #-}

module Generation (generate) where

import Data.Binary (Word16, Word32, Word64, Word8)
import Data.Binary.Put (PutM, putWord16le, putWord32be, putWord32le, putWord64le, putWord8)
import Data.Bits (shift, (.|.))

data ELFHeaderParameter = ELFHeader
  { executionEntryAddress :: Word64,
    programHeadersOffset :: Word64,
    sectionHeadersOffset :: Word64,
    elfHeaderSize :: Word16,
    programHeadersEntrySize :: Word16,
    programHeadersCount :: Word16,
    sectionHeadersEntrySize :: Word16,
    sectionHeadersCount :: Word16,
    sectionHeadersStringIndex :: Word16
  }

pad :: Word8 -> Word -> PutM ()
pad _ 0 = pure ()
pad v n = do
  pad v (n - 1)
  putWord8 v

elfHeader :: ELFHeaderParameter -> PutM ()
elfHeader parameters = do
  -- e_ident
  putWord32be 0x7f_45_4C_46 -- uELF
  putWord8 0x02 -- 64 bit
  putWord8 0x01 -- Little endian
  putWord8 0x01 -- Version 1
  pad 0x00 9

  -- e_type
  putWord16le 0x03 -- Executable

  -- e_machine
  putWord16le 0xB7 -- aarch64
  -- putWord16le 0x00

  -- e_version
  putWord32le 0x01

  -- e_entry
  putWord64le (executionEntryAddress parameters)

  -- e_phoff
  putWord64le (programHeadersOffset parameters)

  -- e_shoff
  putWord64le (sectionHeadersOffset parameters)

  pad 0x00 4

  -- e_ehsize
  putWord16le (elfHeaderSize parameters)

  -- e_phentsize
  putWord16le (programHeadersEntrySize parameters)

  -- e_phnum
  putWord16le (programHeadersCount parameters)

  -- e_shentsize
  putWord16le (sectionHeadersEntrySize parameters)

  -- e_shnum
  putWord16le (sectionHeadersCount parameters)

  -- e_shstrndx
  putWord16le (sectionHeadersStringIndex parameters)

data ELFProgramHeaderParameter = ELFProgramHeader
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

{-
data ELFSectionHeaderParameter = ELFSectionHeader
  { nameOffset :: Word32,
    sectionType :: Word32,
    sectionFlags :: Word64,
    sectionAddress :: Word64,
    sectionOffset :: Word64,
    sectionSize :: Word64
  }

elfSectionHeader parameters = do
  -- sh_name
  putWord32le (nameOffset parameters)

  -- sh_type
  putWord32le (sectionType parameters)

  -- sh_flags
  putWord64le (sectionFlags parameters)

  -- sh_addr
  putWord64le (sectionAddress parameters)

  -- sh_offset
  putWord64le (sectionOffset parameters)

  -- sh_size
  putWord64le (sectionSize parameters)
-}

movzw :: Word8 -> Word16 -> Word32
movzw reg imm = (0x52_80_00_00 :: Word32) .|. shift (fromIntegral imm :: Word32) 5 .|. (fromIntegral reg :: Word32)

movzx :: Word8 -> Word16 -> Word32
movzx reg imm = (0xd2_80_00_00 :: Word32) .|. shift (fromIntegral imm :: Word32) 5 .|. (fromIntegral reg :: Word32)

svc :: Word16 -> Word32
svc imm = (0xd4_00_00_01 :: Word32) .|. shift (fromIntegral imm :: Word32) 5

generateProgram :: PutM ()
generateProgram = do
  putWord32le (movzx 0 4)
  putWord32le (movzw 8 93)
  putWord32le (svc 0)

generate :: Data.Binary.Put.PutM ()
generate = do
  elfHeader (ELFHeader executionEntryAddress (fromIntegral elfHeaderSize) 0 elfHeaderSize programHeadersEntrySize programHeadersCount 0 0 0)
  elfProgramHeader (ELFProgramHeader 0 0 0 programSize programSize)
  generateProgram
  where
    programLength = 12
    elfHeaderSize = 0x40 :: Word16
    programHeadersEntrySize = 0x38
    programHeadersCount = 1
    programHeadersSize = programHeadersEntrySize * programHeadersCount
    executionEntryAddress = fromIntegral (elfHeaderSize + programHeadersSize)
    programSize = fromIntegral (elfHeaderSize + programHeadersSize + programLength)
