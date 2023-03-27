module Parser (Parser, Message, Consumed, nextPos, (<?>), satisfy, many1, parse, finished) where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.List (intercalate)

-- https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parsec-paper-letter.pdf

type Pos = Word

nextPos :: Pos -> Char -> Pos
nextPos p _ = p + 1

data State = S String Pos
  deriving (Show)

data Message = M Pos String [String]

instance Show Message where
  show (M p u es) =
    "parse error at char "
      ++ show p
      ++ ":\n"
      ++ "unexpected "
      ++ u
      ++ "\n"
      ++ "expecting "
      ++ intercalate ", " es

data Reply a = Ok a State Message | Error Message
  deriving (Show)

replyToEither :: Reply b -> Either String b
replyToEither (Ok r _ _) = Right r
replyToEither (Error m) = Left $ show m

data Consumed a = Consumed (Reply a) | Empty (Reply a)
  deriving (Show)

getReply :: Consumed a -> Reply a
getReply (Consumed r) = r
getReply (Empty r) = r

newtype Parser a = P {runParser :: State -> Consumed a}

instance Functor Parser where
  fmap f (P p) = P $ \state -> do
    case p state of
      Empty r -> case r of
        Ok x rest msg -> Empty $ Ok (f x) rest msg
        Error msg -> Empty $ Error msg
      Consumed r -> case r of
        Ok x rest msg -> Consumed $ Ok (f x) rest msg
        Error msg -> Consumed $ Error msg

instance Applicative Parser where
  pure a = P $ \state -> Empty (Ok a state (M 0 "" []))

  -- TODO: Correct implementation?
  P pf <*> P pa = P $ \i ->
    case pf i of
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
  return = pure

  P p >>= f = P $ \i -> case p i of
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
  empty = P $ \(S _ pos) -> Empty (Error (M pos "" []))

  P l <|> P r = P $ \i ->
    case l i of
      Empty (Error msg1) -> case r i of
        Empty (Error msg2) -> mergeError msg1 msg2
        Empty (Ok x inp msg2) -> mergeOk x inp msg1 msg2
        consumed -> consumed
      Empty (Ok x inp msg1) -> case r i of
        Empty (Error msg2) -> mergeError msg1 msg2
        Empty (Ok _ _ msg2) -> mergeOk x inp msg1 msg2
        consumed -> consumed
      consumed -> consumed

expect :: Message -> String -> Message
expect (M pos inp _) exp' = M pos inp [exp']

(<?>) :: Parser a -> String -> Parser a
p <?> exp' = P $ \state ->
  case runParser p state of
    Empty (Error msg) ->
      Empty (Error (expect msg exp'))
    Empty (Ok x st msg) ->
      Empty (Ok x st (expect msg exp'))
    other -> other

finished :: Parser Bool
finished =
  P
    ( \state ->
        let (S i pos) = state
         in case i of
              "" -> Empty (Ok True state (M pos [] []))
              _ -> Empty (Ok False state (M pos [] []))
    )
    <?> "end of input"

satisfy :: (Char -> Bool) -> Parser Char
satisfy test = P $ \(S i pos) ->
  case i of
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

many1 :: (Monad m, Alternative m) => m a -> m [a]
many1 p = do
  x <- p
  xs <- many1 p <|> return []
  return (x : xs)

parse :: String -> Parser a -> Either String a
parse t p = replyToEither $ getReply (runParser p (S t 0))
