module Lib
  ( someFunc,
  )
where

import Lexer (llex)
import System.IO

someFunc :: IO ()
someFunc = do
  putStr "Input lysse code: "
  hFlush stdout
  inp <- getLine
  print (llex inp)
  return ()