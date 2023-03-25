{-# LANGUAGE LambdaCase #-}

module Parser (parse, LAIdentifier (..), LAExpression (..), LAStatement (..)) where

import Control.Applicative (Alternative (..))
import Data.List (nub)
import Lexer (Token (LTEqual, LTIdentifier, LTInteger, LTLBracket, LTMinus, LTOutput, LTPlus, LTRBracket, LTSlash, LTTimes))
import STree
  ( LAExpression (..),
    LAIdentifier (..),
    LAStatement (..),
  )

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

-- If the next token
satisfy :: (t -> Maybe a) -> Parser t a
satisfy p = Parser $ \case
  [] -> Left [EndOfInput]
  hd : rest -> case p hd of
    Just a -> Right (a, rest)
    Nothing -> Left [Unexpected hd]

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

operator :: Parser Token Token
operator = satisfy $ \case
  LTPlus -> Just LTPlus
  LTMinus -> Just LTMinus
  _ -> Nothing

productOperator :: Parser Token Token
productOperator = satisfy $ \case
  LTTimes -> Just LTTimes
  LTSlash -> Just LTSlash
  _ -> Nothing

leftBracket :: Parser Token ()
leftBracket = satisfy $ \case
  LTLBracket -> Just ()
  _ -> Nothing

rightBracket :: Parser Token ()
rightBracket = satisfy $ \case
  LTRBracket -> Just ()
  _ -> Nothing

out :: Parser Token ()
out = satisfy $ \case
  LTOutput -> Just ()
  _ -> Nothing

reference :: Parser Token LAExpression
reference = LAReference <$> identifier

-- https://stackoverflow.com/questions/50369121/bnf-grammar-associativity
-- https://stackoverflow.com/questions/64879983/how-can-i-parse-the-left-associative-notation-of-ski-combinator-calculus
-- https://deepsource.io/blog/monadic-parser-combinators/#combinators-for-repetition
expression :: Parser Token LAExpression
expression = do
  op <- operand
  prods <- many prod
  pop <- foldProds op prods
  terms <- many term
  foldTerms pop terms

term :: Parser Token (Token, LAExpression)
term = do
  to <- operator
  op <- operand
  prods <- many prod
  pop <- foldProds op prods
  return (to, pop)

foldTerms :: LAExpression -> [(Token, LAExpression)] -> Parser Token LAExpression
foldTerms op prods =
  return $
    foldl
      ( \expr (o, t) -> case o of
          LTPlus -> LAAddition expr t
          LTMinus -> LASubtraction expr t
          _ -> error "Unexpected matched token in eend"
      )
      op
      prods

prod :: Parser Token (Token, LAExpression)
prod = do
  po <- productOperator
  op <- operand

  return (po, op)

operand :: Parser Token LAExpression
operand =
  ( do
      leftBracket
      expr <- expression
      rightBracket
      return expr
  )
    <|> number
    <|> reference

foldProds :: LAExpression -> [(Token, LAExpression)] -> Parser Token LAExpression
foldProds op prods =
  return $
    foldl
      ( \expr (o, t) -> case o of
          LTTimes -> LAMultiplication expr t
          LTSlash -> LADivision expr t
          _ -> error "Unexpected matched token in eend"
      )
      op
      prods

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
