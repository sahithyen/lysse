module Lib
  ( someFunc,
  )
where

import System.IO

fsplit :: Char -> String -> (String, String)
fsplit _ "" = ("", "")
fsplit seperator (c : cs)
  | c == seperator = ([], cs)
  | otherwise = (c : n, ns)
  where
    (n, ns) = fsplit seperator cs

split :: Char -> String -> [String]
split _ "" = []
split seperator text = word : split seperator rest
  where
    (word, rest) = fsplit seperator text

someFunc :: IO ()
someFunc = do
  putStr "Input some text: "
  hFlush stdout
  inp <- getLine
  print (split ' ' inp)
  return ()
