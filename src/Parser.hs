{-# LANGUAGE LambdaCase #-}

module Parser () where

import Control.Applicative (Alternative (..))
import Data.List (nub)
import Data.Word (Word64)
import Lexer (Token (LTIdentifier))

data Error i e
  = EndOfInput
  | Unexpected i
  | CustomError e
  | Empty
  deriving (Eq, Show)

newtype Parser i e a = Parser
  {runParser :: [i] -> Either [Error i e] (a, [i])}

instance Functor (Parser i e) where
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    pure (f output, rest)

instance Applicative (Parser i e) where
  pure a = Parser $ \input -> Right (a, input)

  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')

instance Monad (Parser i e) where
  return = pure

  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    runParser (k output) rest

instance (Eq i, Eq e) => Alternative (Parser i e) where
  empty = Parser $ \_ -> Left [Empty]

  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ nub $ err <> err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)

newtype LAIdentifier = LAIdentifier
  {getId :: String}
  deriving (Show, Eq)

data LAExpression
  = LAAddition LAExpression LAExpression
  | LASubtraction LAExpression LAExpression
  | LAInteger Word64
  deriving (Show)

data LAStatement = LAOutput LAIdentifier | LAAssignment LAIdentifier LAExpression
  deriving (Show)

identifier :: Parser Token e LAIdentifier
identifier = Parser $ \case
  [] -> Left [EndOfInput]
  hd : rest -> case hd of
    LTIdentifier str -> Right (LAIdentifier str, rest)
    _ -> Left [Unexpected hd]
