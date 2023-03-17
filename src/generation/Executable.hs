module Executable (generate, RelocationTable) where

import Data.Binary (Word32, Word8, putWord8)
import Data.Binary.Put (PutM, putWord32le)
import Relocation (Relocatable (..), RelocatableWriter, RelocationTable, addRelocatable, executeRelocatableWriter)

addWord :: Word32 -> Relocatable
addWord v = Relocatable (const $ putWord32le v) 4 4

addByte :: Word8 -> Relocatable
addByte v = Relocatable (const $ putWord8 v) 1 1

routine :: RelocatableWriter ()
routine = do
  addRelocatable "code" (addWord 20)
  addRelocatable "code" (addByte 10)
  addRelocatable "code" (addByte 0xff)
  addRelocatable "code" (addByte 0xfe)
  addRelocatable "code" (addByte 0xfd)
  addRelocatable "code" (addByte 0xfc)
  addRelocatable "code" (addWord 20)

  addRelocatable "cata" (addWord 100)

generate :: Data.Binary.Put.PutM ()
generate = do
  putCode rt
  where
    (putCode, rt, _) = executeRelocatableWriter routine
