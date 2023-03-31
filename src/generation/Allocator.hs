module Allocator (allocator, getIdentifierLabel) where

import Data (addLabeledDWord)
import Relocation (RelocatableWriter)
import STree (LAStatement, getIdentifiers)

allocator :: [LAStatement] -> RelocatableWriter ()
allocator stmts = mapM_ allocateIdentifier $ getIdentifiers stmts

allocateIdentifier :: String -> RelocatableWriter ()
allocateIdentifier ident = addLabeledDWord (getIdentifierLabel ident) 0

getIdentifierLabel :: String -> String
getIdentifierLabel ident = "ident@" ++ ident
