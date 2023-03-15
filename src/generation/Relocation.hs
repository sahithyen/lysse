module Relocation (RelocationTable, offsetRelocations, joinRelocationTable) where

import Data.Binary (Word64)
import Data.Map (Map, union)

type RelocationTable = Map String Word64

offsetRelocations :: Word64 -> RelocationTable -> RelocationTable
offsetRelocations a = fmap (+ a)

joinRelocationTable :: RelocationTable -> RelocationTable -> RelocationTable
joinRelocationTable = union
