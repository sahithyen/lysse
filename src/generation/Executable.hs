module Executable (generate, RelocationTable) where

import Data.Binary (Word32, Word8, putWord8)
import Data.Binary.Put (PutM, putWord32le, putWord64le)
import Relocation (Relocatable (..), RelocatableWriter, RelocationTable, addLabel, addRelocatable, executeRelocatableWriter, newRelocatableUnit, offsetRelocations, resolveLabel)

addWord :: Word32 -> Relocatable
addWord v = Relocatable (const $ putWord32le v) 4 4

addByte :: Word8 -> Relocatable
addByte v = Relocatable (const $ putWord8 v) 1 1

writeLabel :: String -> Relocatable
writeLabel l = Relocatable (\rt -> putWord64le (resolveLabel rt l)) 8 8

routine :: RelocatableWriter ()
routine = do
  addRelocatable "code" (addByte 10)
  addRelocatable "code" (addWord 20)
  addRelocatable "code" (addByte 10)
  addRelocatable "code" (addByte 0xff)
  addRelocatable "code" (addByte 0xfe)
  addRelocatable "code" (addByte 0xfd)
  addRelocatable "code" (addByte 0xfc)

  addRelocatable "code" (addWord 20)

  addRelocatable "data" (addByte 0xff)
  newRelocatableUnit "data"
  addLabel "data" "some"
  addRelocatable "data" (addWord 0xcafecafe)

  addRelocatable "test" (writeLabel "some")

generate :: Data.Binary.Put.PutM ()
generate = do
  putCode newRt
  where
    (putCode, rt, _) = executeRelocatableWriter routine
    newRt = offsetRelocations 0 rt
