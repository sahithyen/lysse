module Lib
  ( someFunc,
  )
where

import Data.Binary.Put (runPut)
import Data.ByteString.Lazy as B (writeFile)
import Generation (generate)
import Lexer (llex)
import Parser (parse)
import System.IO (hFlush, stdout)

someFunc :: IO ()
someFunc = do
  putStr "Input lysse code: "
  hFlush stdout
  inp <- getLine
  print (parse (llex inp))

  B.writeFile "lysse.bin" (runPut generate)

  return ()