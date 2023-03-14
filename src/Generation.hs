{-# LANGUAGE NumericUnderscores #-}

module Generation (generate) where

import Control.Monad.State (execStateT, get, put)
import Control.Monad.State.Lazy (StateT)
import Control.Monad.Writer.Lazy
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

type RelocatableInstruction = RelocationTable -> Word32

type RoutineState = (Word64, RelocationTable)

type RoutineWriter = StateT RoutineState (Writer [RelocatableInstruction])

addRelocatableInstruction :: RelocatableInstruction -> RoutineWriter ()
addRelocatableInstruction instruction = do
  (cl, rt) <- get
  put (cl + 4, rt)
  tell $ pure instruction

addInstruction :: Word32 -> RoutineWriter ()
addInstruction = addRelocatableInstruction . const

addLabel :: String -> RoutineWriter ()
addLabel label = do
  (cl, rt) <- get
  put (cl, insert label cl rt)

getLabel :: String -> RoutineWriter Word64
getLabel l = do
  (_, rt) <- get
  case Data.Map.lookup l rt of
    Just address -> do return address
    _ -> error ("Label '" ++ l ++ "' not found in relocation table")

movzw :: Word8 -> Word16 -> RoutineWriter ()
movzw reg imm = addInstruction $ (0x52_80_00_00 :: Word32) .|. shift (fromIntegral imm :: Word32) 5 .|. (fromIntegral reg :: Word32)

movzx :: Word8 -> Word16 -> RoutineWriter ()
movzx reg imm = addInstruction $ (0xd2_80_00_00 :: Word32) .|. shift (fromIntegral imm :: Word32) 5 .|. (fromIntegral reg :: Word32)

svc :: Word16 -> RoutineWriter ()
svc imm = addInstruction $ (0xd4_00_00_01 :: Word32) .|. shift (fromIntegral imm :: Word32) 5

b :: String -> RoutineWriter ()
b label = do
  address <- getLabel label
  addInstruction $ fromIntegral address

r0 :: Word8
r0 = 0

r8 :: Word8
r8 = 8

exitRoutine :: Word16 -> RoutineWriter ()
exitRoutine exitCode = do
  movzx r0 exitCode
  addLabel "hello"
  movzw r8 93
  svc 0
  b "hello"

putRoutine :: [Word32] -> PutM ()
putRoutine [] = pure ()
putRoutine (x : xs) = do
  putWord32le x
  putRoutine xs

relocate :: [RelocatableInstruction] -> RelocationTable -> [Word32]
relocate routine rt = routine <*> pure rt

generate :: Data.Binary.Put.PutM ()
generate = do
  elfHeader (ELFHeader executionEntryAddress programHeadersEntrySize programHeadersCount)
  elfProgramHeader (ELFProgramHeader programFileOffset loadAddress loadAddress programFileSize programFileSize)
  putRoutine program
  where
    ((programSize, relocationTable), relocatableProgram) = runWriter (execStateT (exitRoutine 4) (0, empty))
    program = relocate relocatableProgram relocationTable
    elfHeaderSize = 0x40
    programHeadersEntrySize = 0x38
    programHeadersCount = 1
    programHeadersSize = programHeadersEntrySize * programHeadersCount
    headerSize = fromIntegral (elfHeaderSize + programHeadersSize)
    programFileOffset = 0 -- Offset in file needs to be a multiple of the page size (page size in arm: 4096 bytes)
    programFileSize = headerSize + programSize
    loadAddress = 0
    executionEntryAddress = loadAddress + headerSize
