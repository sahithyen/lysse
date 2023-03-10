module Lexer (Token (..), llex) where

import Data.Char (isDigit, isLower)
import Data.Word (Word64)

data Token
  = LTIdentifier String
  | LTInteger Word64
  | LTEqual
  | LTPlus
  | LTMinus
  | LTOutput
  deriving
    (Show)

takeIdentifier :: String -> (String, String)
takeIdentifier "" = ("", "")
takeIdentifier (x : xs)
  | isLower x = (x : identifier, rest)
  | otherwise = ("", x : xs)
  where
    (identifier, rest) = takeIdentifier xs

takeInteger :: String -> (String, String)
takeInteger "" = ("", "")
takeInteger (x : xs)
  | isDigit x = (x : integer, rest)
  | otherwise = ("", x : xs)
  where
    (integer, rest) = takeInteger xs

getToken :: [Token] -> String -> ([Token], String)
getToken tokens "" = (tokens, "")
getToken tokens (x : xs)
  | isLower x = let (name, rest) = takeIdentifier (x : xs) in getToken (tokens ++ [LTIdentifier name]) rest
  | isDigit x = let (integer, rest) = takeInteger (x : xs) in getToken (tokens ++ [LTInteger (read integer)]) rest
  | x == '=' = getToken (tokens ++ [LTEqual]) xs
  | x == '+' = getToken (tokens ++ [LTPlus]) xs
  | x == '-' = getToken (tokens ++ [LTMinus]) xs
  | x == '>' = getToken (tokens ++ [LTOutput]) xs
  | otherwise = getToken tokens xs

llex :: String -> [Token]
llex = fst . getToken []
