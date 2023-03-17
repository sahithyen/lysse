module Lib
  ( someFunc,
    compile,
  )
where

import Data.Binary.Put (runPut)
import Data.ByteString.Lazy as B (writeFile)
import Executable (generate)
import Lexer (llex)
import Parser (parse)
import System.IO (hFlush, stdout)

someFunc :: IO ()
someFunc = do
  B.writeFile "ly" (runPut generate)

compile :: IO ()
compile = do
  putStr "Input lysse code: "
  hFlush stdout
  inp <- getLine
  print (parse (llex inp))