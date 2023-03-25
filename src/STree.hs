module STree (LAIdentifier (..), LAExpression (..), LAStatement (..), getIdentifiers) where

import Data.Binary (Word64)

newtype LAIdentifier = LAIdentifier
  {getId :: String}
  deriving (Show, Eq)

data LAExpression
  = LAAddition LAExpression LAExpression
  | LASubtraction LAExpression LAExpression
  | LAReference LAIdentifier
  | LAInteger Word64
  deriving (Show, Eq)

data LAStatement = LAInput LAIdentifier | LAOutput LAIdentifier | LAAssignment LAIdentifier LAExpression
  deriving (Show, Eq)

-- Identifier
getIdentifiers :: [LAStatement] -> [String]
getIdentifiers = concatMap getDefinedIdentifiers

getDefinedIdentifiers :: LAStatement -> [String]
getDefinedIdentifiers (LAInput _) = []
getDefinedIdentifiers (LAOutput _) = []
getDefinedIdentifiers (LAAssignment (LAIdentifier ident) _) = [ident]