module Code_amd64 (createLabel, label) where

import Relocation (RelocatableWriter, addLabel, getUniqueLabel)

createLabel :: RelocatableWriter String
createLabel = getUniqueLabel

label :: String -> RelocatableWriter ()
label = addLabel "code"