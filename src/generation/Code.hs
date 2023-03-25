module Code (createLabel, label, call, stacked) where

import Data.Binary (Word32)
import Instructions (bl, ldri, sp, stripre)
import Relocation (RelocatableWriter, addLabel, getUniqueLabel)

createLabel :: RelocatableWriter String
createLabel = getUniqueLabel

label :: String -> RelocatableWriter ()
label = addLabel "code"

call :: [Word32] -> String -> RelocatableWriter ()
call rs l = stacked rs $ bl l

stacked :: [Word32] -> RelocatableWriter () -> RelocatableWriter ()
stacked rs w = do
  mapM_ (\r -> stripre r sp (-16)) rs
  w
  mapM_ (\r -> ldri r sp 16) (reverse rs)
