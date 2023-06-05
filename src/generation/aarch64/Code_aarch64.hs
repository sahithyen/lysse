module Code_aarch64 (call, stacked) where

import Data.Binary (Word32)
import Instructions_aarch64 (bl, ldri, sp, stripre)
import Relocation (RelocatableWriter)

call :: [Word32] -> String -> RelocatableWriter ()
call rs l = stacked rs $ bl l

stacked :: [Word32] -> RelocatableWriter () -> RelocatableWriter ()
stacked rs w = do
  mapM_ (\r -> stripre r sp (-16)) rs
  w
  mapM_ (\r -> ldri r sp 16) (reverse rs)
