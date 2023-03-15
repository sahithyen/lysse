module DataWriter (LData (..), DataTable, DataWriter, writeData, lword, offsetPutData) where

import Control.Monad.State (StateT, execStateT, gets, modify)
import Control.Monad.Writer (MonadWriter (tell), Writer, runWriter)
import Data.Binary (Word32, Word64)
import Data.Binary.Put (Put, putWord32le)
import Data.Map (Map, empty, foldrWithKey, insert)
import Relocation (RelocationTable, offsetRelocations)

data LData = LData {dataWriter :: Put, dataLength :: Word64}

type DataTable = Map String LData

data DataState = DataState {dataSize :: Word64, relocationTable :: RelocationTable}

setDataSize :: Word64 -> DataState -> DataState
setDataSize value state = state {dataSize = value}

setRelocationTable :: RelocationTable -> DataState -> DataState
setRelocationTable value state = state {relocationTable = value}

type DataWriter = StateT DataState (Writer [Put])

addLabel :: String -> DataWriter ()
addLabel label = do
  cl <- gets dataSize
  rt <- gets relocationTable
  modify $ setRelocationTable (insert label cl rt)

writeData :: String -> LData -> DataWriter ()
writeData l d = do
  -- Add label
  addLabel l

  -- Increase data size
  ds <- gets dataSize
  modify $ setDataSize (ds + dataLength d)

  -- Write data
  tell $ pure (dataWriter d)

writeDataEl :: String -> LData -> DataWriter () -> DataWriter ()
writeDataEl l d w = do
  w
  writeData l d

writeDataTable :: DataTable -> DataWriter ()
writeDataTable = Data.Map.foldrWithKey writeDataEl (pure ())

lword :: Word32 -> LData
lword w = LData (putWord32le w) 4

initialDataState :: DataState
initialDataState = DataState 0 empty

execute :: DataTable -> (DataState, [Put])
execute dt = runWriter (execStateT (writeDataTable dt) initialDataState)

putAll :: [Put] -> Put
putAll [] = pure ()
putAll (p : ps) = do
  p
  putAll ps

offsetPutData :: Word64 -> DataTable -> (Put, RelocationTable)
offsetPutData offset dt = (w, offRt)
  where
    (s, ps) = execute dt
    w = putAll ps
    rt = relocationTable s
    offRt = offsetRelocations offset rt
