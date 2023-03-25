module Data (addByte, addWord, addDWord, addLabeledDWord, addString) where

import Data.Binary (Word32, Word64, Word8, putWord8)
import Data.Binary.Put (Put, putLazyByteString, putWord32le, putWord64le)
import Data.ByteString.Lazy as BS (length)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Relocation (Relocatable (..), RelocatableWriter, addLabeledRelocatable, getUniqueLabel)

addLabeledData :: String -> Put -> Word64 -> Word64 -> RelocatableWriter ()
addLabeledData l p s a = do
  addLabeledRelocatable "data" l $ Relocatable (const p) s a

addData :: Put -> Word64 -> Word64 -> RelocatableWriter String
addData p s a = do
  l <- getUniqueLabel
  addLabeledData l p s a
  return l

addByte :: Word8 -> RelocatableWriter String
addByte d = addData (putWord8 d) 1 1

addWord :: Word32 -> RelocatableWriter String
addWord d = addData (putWord32le d) 4 4

addLabeledDWord :: String -> Word64 -> RelocatableWriter ()
addLabeledDWord l d = do
  addLabeledData l (putWord64le d) 8 8

addDWord :: Word64 -> RelocatableWriter String
addDWord d = addData (putWord64le d) 8 8

addString :: String -> RelocatableWriter (String, Word64)
addString s = do
  l <- addData (putLazyByteString bs) bsl 8
  return (l, bsl)
  where
    bs = encodeUtf8 . pack $ s
    bsl = fromIntegral $ BS.length bs
