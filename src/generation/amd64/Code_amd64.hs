{-# LANGUAGE ImportQualifiedPost #-}

module Code_amd64 (call, stacked) where

import Instructions_amd64 (Register, pop, push)
import Instructions_amd64 qualified as I (call)
import Relocation (RelocatableWriter)

call :: [Register] -> String -> RelocatableWriter ()
call rs l = stacked rs $ I.call l

stacked :: [Register] -> RelocatableWriter () -> RelocatableWriter ()
stacked rs w = do
  mapM_ push rs
  w
  mapM_ pop (reverse rs)
