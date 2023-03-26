module LyParser (identifier) where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Char (isAlpha, isDigit)

-- https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parsec-paper-letter.pdf

type Pos = Word

nextPos :: Pos -> Char -> Pos
nextPos p _ = p + 1

data State = S String Pos
  deriving (Show)

data Message = M Pos String [String]
  deriving (Show)

data Reply a = Ok a State Message | Error Message
  deriving (Show)

data Consumed a = Consumed (Reply a) | Empty (Reply a)
  deriving (Show)

newtype Parser a = P {runParser :: State -> Consumed a}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (P p) = P $ \state -> do
    case p state of
      Empty r -> case r of
        Ok x rest msg -> Empty $ Ok (f x) rest msg
        Error msg -> Empty $ Error msg
      Consumed r -> case r of
        Ok x rest msg -> Consumed $ Ok (f x) rest msg
        Error msg -> Consumed $ Error msg

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = P $ \state -> Empty (Ok a state (M 0 "" []))

  -- TODO: Correct implementation?
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  P pf <*> P pa = P $ \input ->
    case pf input of
      Empty r -> case r of
        Ok f rest _ -> case pa rest of
          Empty r2 -> case r2 of
            Ok a rest2 msg2 -> Empty $ Ok (f a) rest2 msg2
            Error msg2 -> Empty (Error msg2)
          Consumed r2 -> case r2 of
            Ok a rest2 msg2 -> Consumed $ Ok (f a) rest2 msg2
            Error msg2 -> Consumed $ Error msg2
        Error msg -> Empty $ Error msg
      Consumed r -> Consumed $ case r of
        Ok f rest _ -> case pa rest of
          Empty r2 -> case r2 of
            Ok a rest2 msg2 -> Ok (f a) rest2 msg2
            Error msg2 -> Error msg2
          Consumed r2 -> case r2 of
            Ok a rest2 msg2 -> Ok (f a) rest2 msg2
            Error msg2 -> Error msg2
        Error msg -> Error msg

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  P p >>= f = P $ \input -> case p input of
    Empty reply1 ->
      case reply1 of
        Ok x rest _ -> runParser (f x) rest
        Error msg -> Empty (Error msg)
    Consumed reply1 ->
      Consumed
        ( case reply1 of
            Ok x rest _ ->
              case runParser (f x) rest of
                Consumed reply2 -> reply2
                Empty reply2 -> reply2
            Error msg -> Error msg
        )

merge :: Message -> Message -> Message
merge (M pos inp exp1) (M _ _ exp2) = M pos inp (exp1 ++ exp2)

mergeOk :: a -> State -> Message -> Message -> Consumed a
mergeOk x inp msg1 msg2 = Empty (Ok x inp (merge msg1 msg2))

mergeError :: Message -> Message -> Consumed a
mergeError msg1 msg2 = Empty (Error (merge msg1 msg2))

instance Alternative Parser where
  empty :: Parser a
  empty = P $ \(S _ pos) -> Empty (Error (M pos "" []))

  (<|>) :: Parser a -> Parser a -> Parser a
  P l <|> P r = P $ \input ->
    case l input of
      Empty (Error msg1) -> case r input of
        Empty (Error msg2) -> mergeError msg1 msg2
        Empty (Ok x inp msg2) -> mergeOk x inp msg1 msg2
        consumed -> consumed
      Empty (Ok x inp msg1) -> case r input of
        Empty (Error msg2) -> mergeError msg1 msg2
        Empty (Ok _ _ msg2) -> mergeOk x inp msg1 msg2
        consumed -> consumed
      consumed -> consumed

(<?>) :: Parser a -> String -> Parser a
p <?> exp' = P $ \state ->
  case runParser p state of
    Empty (Error msg) ->
      Empty (Error (expect msg exp'))
    Empty (Ok x st msg) ->
      Empty (Ok x st (expect msg exp'))
    other -> other

expect :: Message -> String -> Message
expect (M pos inp _) exp' = M pos inp [exp']

satisfy :: (Char -> Bool) -> Parser Char
satisfy test = P $ \(S input pos) ->
  case input of
    (c : cs)
      | test c ->
          let newPos = nextPos pos c
              newState = S cs newPos
           in seq
                newPos
                ( Consumed
                    ( Ok
                        c
                        newState
                        (M pos [] [])
                    )
                )
      | otherwise -> Empty (Error (M pos [c] []))
    [] -> Empty (Error (M pos "end of input" []))

char :: Char -> Parser Char
char c = satisfy (== c) <?> show c

letter :: Parser Char
letter = satisfy isAlpha <?> "letter"

digit :: Parser Char
digit = satisfy isDigit <?> "digit"

string :: String -> Parser ()
string [] = pure ()
string (c : cs) =
  do
    _ <- char c
    string cs <?> (c : cs)

many1 :: (Monad m, Alternative m) => m a -> m [a]
many1 p = do
  x <- p
  xs <- many1 p <|> return []
  return (x : xs)

try :: Parser a -> Parser a
try (P p) = P $ \input -> case p input of
  Consumed (Error msg) -> Empty (Error msg)
  other -> other

identifier :: Parser [Char]
identifier = many1 (letter <|> digit <|> (char '_' <?> "underscore"))