module Instructions_amd64 () where

import Data.Binary (Put, Word64)
import Data.Binary.Put (putWord16le)
import Relocation (Relocatable (Relocatable), RelocatableWriter, RelocationTable, addRelocatable)

addRelocatableInstruction :: (RelocationTable -> Put) -> Word64 -> Word64 -> RelocatableWriter ()
addRelocatableInstruction fn s a = addRelocatable "code" (Relocatable fn s a)

addInstruction :: Put -> Word64 -> Word64 -> RelocatableWriter ()
addInstruction p = addRelocatableInstruction (const p)

nop :: RelocatableWriter ()
nop = addInstruction (putWord16le 0x90) 1 1

syscall :: RelocatableWriter ()
syscall = addInstruction (putWord16le 0x0f05) 2 1