module RoutineWriter (RoutineWriter, addLabel, getLabel, addInstruction, addUniqueLabel, addRelocatableInstruction, assembleRoutine, relocationTable, relocate, programSize, addWord, getRelativeAddress, dataTable) where

import Control.Monad.State (execStateT, gets, modify)
import Control.Monad.State.Lazy (StateT)
import Control.Monad.Writer.Lazy
  ( MonadWriter (tell),
    Writer,
    runWriter,
  )
import Data.Binary (Word32, Word64)
import Data.Map (empty, insert, lookup)
import DataWriter (DataTable, LData, lword)
import Relocation (RelocationTable)

type RelocatableInstruction = RelocationTable -> Word32

relocate :: [RelocatableInstruction] -> RelocationTable -> [Word32]
relocate routine rt = routine <*> pure rt

data RoutineState = RoutineState {programSize :: Word64, relocationTable :: RelocationTable, dataTable :: DataTable}

setProgramSize :: Word64 -> RoutineState -> RoutineState
setProgramSize value state = state {programSize = value}

setRelocationTable :: RelocationTable -> RoutineState -> RoutineState
setRelocationTable value state = state {relocationTable = value}

setDataTable :: DataTable -> RoutineState -> RoutineState
setDataTable value state = state {dataTable = value}

initialRoutineState :: RoutineState
initialRoutineState = RoutineState 0 empty empty

type RoutineWriter = StateT RoutineState (Writer [RelocatableInstruction])

addRelocatableInstruction :: RelocatableInstruction -> RoutineWriter ()
addRelocatableInstruction instruction = do
  cl <- gets programSize
  modify (setProgramSize (cl + 4))
  tell $ pure instruction

addInstruction :: Word32 -> RoutineWriter ()
addInstruction = addRelocatableInstruction . const

addLabel :: String -> RoutineWriter ()
addLabel label = do
  cl <- gets programSize
  rt <- gets relocationTable
  modify $ setRelocationTable (insert label cl rt)

addUniqueLabel :: RoutineWriter String
addUniqueLabel = do
  cl <- gets programSize
  addLabel $ "tmp_" ++ show cl
  return $ "tmp_" ++ show cl

addLData :: String -> LData -> RoutineWriter ()
addLData l d = do
  dt <- gets dataTable
  modify $ setDataTable (insert l d dt)

addWord :: String -> Word32 -> RoutineWriter ()
addWord l w = addLData l (lword w)

getLabel :: RelocationTable -> String -> Word64
getLabel rt l = case Data.Map.lookup l rt of
  Just address -> address
  _ -> error ("Label '" ++ l ++ "' not found in relocation table")

getRelativeAddress :: RelocationTable -> String -> String -> Word64
getRelativeAddress rt dest_label from_label = getLabel rt dest_label - getLabel rt from_label

assembleRoutine :: RoutineWriter () -> (RoutineState, [RelocatableInstruction])
assembleRoutine routine = runWriter (execStateT routine initialRoutineState)