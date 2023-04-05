module LyParser (parseLy) where

import Control.Applicative (Alternative (many), (<|>))
import Data.Char (isAlpha, isDigit)
import Parser (Parser, finished, many1, parse, satisfy, (<?>))
import STree
  ( LAExpression (..),
    LAIdentifier (LAIdentifier),
    LAStatement (..),
  )

char :: Char -> Parser Char
char c = satisfy (== c) <?> show c

letter :: Parser Char
letter = satisfy isAlpha <?> "letter"

digit :: Parser Char
digit = satisfy isDigit <?> "digit"

whitespace :: Parser ()
whitespace = do
  _ <- (char ' ' <?> "space") <|> (char '\n' <?> "space")
  return ()

-- Tokens
whitespaces :: Parser ()
whitespaces = do
  _ <- many whitespace
  return () <?> "whitespace"

identifier :: Parser LAIdentifier
identifier =
  ( do
      x <- letter <|> (char '_' <?> "underscore")
      xs <- many (letter <|> digit <|> (char '_' <?> "underscore"))
      let ident = LAIdentifier (x : xs)
      return ident
  )
    <?> "identifier"

integer :: Parser Int
integer =
  do
    i <- many1 digit
    return (read i) <?> "integer"

equal :: Parser ()
equal = do
  _ <- char '='
  return ()

data TermOperator = Plus | Minus

plus :: Parser TermOperator
plus = do
  _ <- char '+'
  return Plus

minus :: Parser TermOperator
minus = do
  _ <- char '-'
  return Minus

data FactorOperator = Times | Divide

times :: Parser FactorOperator
times = do
  _ <- char '*'
  return Times

slash :: Parser FactorOperator
slash = do
  _ <- char '/'
  return Divide

leftBracket :: Parser ()
leftBracket = do
  _ <- char '('
  return ()

rightBracket :: Parser ()
rightBracket = do
  _ <- char ')'
  return ()

output :: Parser ()
output = do
  _ <- char '>'
  return ()

input :: Parser ()
input = do
  _ <- char '<'
  return ()

-- Rules

number :: Parser LAExpression
number = (LAInteger . fromIntegral <$> integer) <?> "number"

reference :: Parser LAExpression
reference = (LAReference <$> identifier) <?> "reference"

operand :: Parser LAExpression
operand =
  bracketedExpression
    <|> number
    <|> reference

-- https://stackoverflow.com/questions/50369121/bnf-grammar-associativity
-- https://stackoverflow.com/questions/64879983/how-can-i-parse-the-left-associative-notation-of-ski-combinator-calculus
-- https://deepsource.io/blog/monadic-parser-combinators/#combinators-for-repetition
expression :: Parser LAExpression
expression = do
  op <- operand

  whitespaces

  prods <- many prod
  pop <- foldProds op prods

  whitespaces

  terms <- many term
  foldTerms pop terms

bracketedExpression :: Parser LAExpression
bracketedExpression =
  ( do
      leftBracket
      whitespaces
      expr <- expression
      whitespaces
      rightBracket
      return expr
  )
    <?> "bracketed expression"

termOperator :: Parser TermOperator
termOperator = plus <|> minus

term :: Parser (TermOperator, LAExpression)
term = do
  to <- termOperator
  whitespaces

  op <- operand
  whitespaces

  prods <- many prod
  pop <- foldProds op prods
  return (to, pop)

foldTerms :: LAExpression -> [(TermOperator, LAExpression)] -> Parser LAExpression
foldTerms op prods =
  return $
    foldl
      ( \expr (o, t) -> case o of
          Plus -> LAAddition expr t
          Minus -> LASubtraction expr t
      )
      op
      prods

factorOperator :: Parser FactorOperator
factorOperator = times <|> slash

prod :: Parser (FactorOperator, LAExpression)
prod = do
  po <- factorOperator

  whitespaces

  op <- operand

  whitespaces

  return (po, op)

foldProds :: LAExpression -> [(FactorOperator, LAExpression)] -> Parser LAExpression
foldProds op prods =
  return $
    foldl
      ( \expr (o, t) -> case o of
          Times -> LAMultiplication expr t
          Divide -> LADivision expr t
      )
      op
      prods

assignmentStatement :: Parser LAStatement
assignmentStatement =
  ( do
      ident <- identifier
      whitespaces

      equal
      whitespaces

      LAAssignment ident <$> expression
  )
    <?> "assignment"

inputStatement :: Parser LAStatement
inputStatement =
  ( do
      input
      whitespaces
      LAInput <$> identifier
  )
    <?> "input command"

outputStatement :: Parser LAStatement
outputStatement =
  ( do
      output
      whitespaces
      LAOutput <$> identifier
  )
    <?> "output command"

statement :: Parser LAStatement
statement = assignmentStatement <|> outputStatement <|> inputStatement

statements :: Parser [LAStatement]
statements = do
  whitespaces
  f <- finished
  if f
    then do
      return []
    else do
      stmt <- statement

      stmts <- statements
      return (stmt : stmts)

parseLy :: String -> Either String [LAStatement]
parseLy t = parse t statements
