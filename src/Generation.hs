{-# LANGUAGE NumericUnderscores #-}

module Generation (generate) where

import Data.Binary (Word16, Word32, Word64, Word8)
import Data.Binary.Put (PutM, putWord16le, putWord32be, putWord32le, putWord64le, putWord8)
import Data.Bits (shift, (.|.))
import Data.Map

data ELFHeaderParameter = ELFHeader
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

type RelocationTable = Map String Word64

newtype Relocatable a = Reloc (RelocationTable -> a)

instance Functor Relocatable where
  fmap f (Reloc r) = Reloc $ \rt -> f (r rt)

instance Applicative Relocatable where
  pure a = Reloc $ const a

  Reloc f <*> Reloc p = Reloc $ \rt -> f rt (p rt)

instance Monad Relocatable where
  return = pure

  p >>= k = Reloc (\rt -> relocate (k (relocate p rt)) rt)

relocate :: Relocatable a -> RelocationTable -> a
relocate (Reloc fn) = fn

movzw :: Word8 -> Word16 -> Relocatable Word32
movzw reg imm = Reloc (\_ -> (0x52_80_00_00 :: Word32) .|. shift (fromIntegral imm :: Word32) 5 .|. (fromIntegral reg :: Word32))

movzx :: Word8 -> Word16 -> Relocatable Word32
movzx reg imm = Reloc (\_ -> (0xd2_80_00_00 :: Word32) .|. shift (fromIntegral imm :: Word32) 5 .|. (fromIntegral reg :: Word32))

svc :: Word16 -> Relocatable Word32
svc imm = Reloc (\_ -> (0xd4_00_00_01 :: Word32) .|. shift (fromIntegral imm :: Word32) 5)

r0 :: Word8
r0 = 0

r8 :: Word8
r8 = 8

routine :: Functor f => f (Relocatable b) -> Relocatable (f b)
routine a = Reloc (\rt -> fmap (`relocate` rt) a)

exitRoutine :: Word16 -> Relocatable [Word32]
exitRoutine exitCode =
  routine
    [ movzx r0 exitCode,
      movzw r8 93,
      svc 0
    ]

putRoutine :: [Word32] -> PutM ()
putRoutine [] = pure ()
putRoutine (x : xs) = do
  putWord32le x
  putRoutine xs

generate :: Data.Binary.Put.PutM ()
generate = do
  elfHeader (ELFHeader executionEntryAddress programHeadersEntrySize programHeadersCount)
  elfProgramHeader (ELFProgramHeader programFileOffset loadAddress loadAddress programSize programSize)
  putRoutine (relocate (exitRoutine 4) empty)
  where
    elfHeaderSize = 0x40
    programHeadersEntrySize = 0x38
    programHeadersCount = 1
    programHeadersSize = programHeadersEntrySize * programHeadersCount
    headerSize = fromIntegral (elfHeaderSize + programHeadersSize)
    programFileOffset = 0 -- Offset in file needs to be a multiple of the page size (page size in arm: 4096 bytes)
    programSize = headerSize + 12
    loadAddress = 0
    executionEntryAddress = loadAddress + headerSize
