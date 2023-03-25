module STree (LAIdentifier (..), LAExpression (..), LAStatement (..), showStatements, getIdentifiers) where

import Data.Binary (Word64)

newtype LAIdentifier = LAIdentifier
  {getId :: String}
  deriving (Show, Eq)

data LAExpression
  = LAAddition LAExpression LAExpression
  | LASubtraction LAExpression LAExpression
  | LAMultiplication LAExpression LAExpression
  | LADivision LAExpression LAExpression
  | LAReference LAIdentifier
  | LAInteger Word64
  deriving (Show, Eq)

data LAStatement = LAInput LAIdentifier | LAOutput LAIdentifier | LAAssignment LAIdentifier LAExpression
  deriving (Eq)

-- Show

showStatements :: [LAStatement] -> String
showStatements = concatMap (\x -> show x ++ "\n")

instance Show LAStatement where
  show stmt = case stmt of
    (LAInput ident) -> "(<) " ++ show (getId ident)
    (LAOutput ident) -> "(>) " ++ show (getId ident)
    (LAAssignment ident expr) -> "(=) " ++ show (getId ident) ++ "\n" ++ showExprAtLevel 4 expr
    where
      showExprAtLevel l (LAInteger i) = addSpace l ++ show i
      showExprAtLevel l (LAReference r) = addSpace l ++ getId r
      showExprAtLevel l (LAAddition a_op b_op) = addSpace l ++ "(+) " ++ "\n" ++ showExprAtLevel (l + 4) a_op ++ "\n" ++ showExprAtLevel (l + 4) b_op
      showExprAtLevel l (LASubtraction a_op b_op) = addSpace l ++ "(-) " ++ "\n" ++ showExprAtLevel (l + 4) a_op ++ "\n" ++ showExprAtLevel (l + 4) b_op
      showExprAtLevel l (LAMultiplication a_op b_op) = addSpace l ++ "(*) " ++ "\n" ++ showExprAtLevel (l + 4) a_op ++ "\n" ++ showExprAtLevel (l + 4) b_op
      showExprAtLevel l (LADivision a_op b_op) = addSpace l ++ "(/) " ++ "\n" ++ showExprAtLevel (l + 4) a_op ++ "\n" ++ showExprAtLevel (l + 4) b_op

addSpace :: Int -> [Char]
addSpace = flip replicate ' '

-- Identifier
getIdentifiers :: [LAStatement] -> [String]
getIdentifiers = concatMap getDefinedIdentifiers

getDefinedIdentifiers :: LAStatement -> [String]
getDefinedIdentifiers (LAInput _) = []
getDefinedIdentifiers (LAOutput _) = []
getDefinedIdentifiers (LAAssignment (LAIdentifier ident) _) = [ident]