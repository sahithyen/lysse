{-# LANGUAGE LambdaCase #-}

module Parser (parse, LAIdentifier (..), LAExpression (..), LAStatement (..)) where

import Control.Applicative (Alternative (..))
import Data.List (nub)
import Data.Word (Word64)
import Lexer (Token (LTEqual, LTIdentifier, LTInteger, LTMinus, LTOutput, LTPlus))

data Error i
  = EndOfInput
  | Unexpected i
  | Empty
  deriving (Eq, Show)

newtype Parser i a = Parser
  {runParser :: [i] -> Either [Error i] (a, [i])}

instance Functor (Parser i) where
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    pure (f output, rest)

instance Applicative (Parser i) where
  pure a = Parser $ \input -> Right (a, input)

  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')

instance Monad (Parser i) where
  return = pure

  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    runParser (k output) rest

instance Eq i => Alternative (Parser i) where
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
  | LAReference LAIdentifier
  | LAInteger Word64
  deriving (Show)

data LAStatement = LAOutput LAIdentifier | LAAssignment LAIdentifier LAExpression
  deriving (Show)

-- If the next token
satisfy :: (t -> Maybe a) -> Parser t a
satisfy p = Parser $ \case
  [] -> Left [EndOfInput]
  hd : rest -> case p hd of
    Just a -> Right (a, rest)
    Nothing -> Left [Unexpected hd]

may :: (t -> Maybe a) -> Parser t (Maybe a)
may p = Parser $ \case
  [] -> Right (Nothing, [])
  hd : rest -> case p hd of
    Just a -> Right (Just a, rest)
    Nothing -> Right (Nothing, hd : rest)

finished :: Parser i Bool
finished = Parser $ \case
  [] -> Right (True, [])
  ts -> Right (False, ts)

identifier :: Parser Token LAIdentifier
identifier = satisfy $ \case
  LTIdentifier str -> Just (LAIdentifier str)
  _ -> Nothing

number :: Parser Token LAExpression
number = satisfy $ \case
  LTInteger num -> Just (LAInteger num)
  _ -> Nothing

equal :: Parser Token ()
equal = satisfy $ \case
  LTEqual -> Just ()
  _ -> Nothing

mayOperator :: Parser Token (Maybe Token)
mayOperator = may $ \case
  LTPlus -> Just LTPlus
  LTMinus -> Just LTMinus
  _ -> Nothing

out :: Parser Token ()
out = satisfy $ \case
  LTOutput -> Just ()
  _ -> Nothing

reference :: Parser Token LAExpression
reference = LAReference <$> identifier

expression :: Parser Token LAExpression
expression = do
  expr <- estart
  eend expr

estart :: Parser Token LAExpression
estart = number <|> reference

eend :: LAExpression -> Parser Token LAExpression
eend expr = do
  res <- mayOperator
  case res of
    Nothing -> pure expr
    Just LTPlus -> LAAddition expr <$> expression
    Just LTMinus -> LASubtraction expr <$> expression
    Just _ -> error "Unexpected matched token in eend"

assignmentStatement :: Parser Token LAStatement
assignmentStatement = do
  ident <- identifier
  equal
  LAAssignment ident <$> expression

outputStatement :: Parser Token LAStatement
outputStatement = do
  out
  LAOutput <$> identifier

statement :: Parser Token LAStatement
statement = assignmentStatement <|> outputStatement

statements :: Parser Token [LAStatement]
statements = do
  f <- finished
  if f
    then do
      return []
    else do
      stmt <- statement
      stmts <- statements
      return (stmt : stmts)

parse :: [Token] -> Either [Error Token] ([LAStatement], [Token])
parse = runParser statements
