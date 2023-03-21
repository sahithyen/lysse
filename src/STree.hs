module STree (LAIdentifier (..), LAExpression (..), LAStatement (..)) where

import Data.Binary (Word64)

newtype LAIdentifier = LAIdentifier
  {getId :: String}
  deriving (Show, Eq)

data LAExpression
  = LAAddition LAExpression LAExpression
  | LASubtraction LAExpression LAExpression
  | LAReference LAIdentifier
  | LAInteger Word64
  deriving (Show)

data LAStatement = LAOutput LAIdentifier | LAAssignment LAIdentifier LAExpression
  deriving (Show)
