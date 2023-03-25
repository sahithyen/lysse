module Lib
  ( compile,
  )
where

import Data.Binary.Put (runPut)
import Data.ByteString.Lazy as B (writeFile)
import Executable (generate)
import Lexer (llex)
import LyGen (lysseProgram)
import Parser (parse)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

compile :: IO ()
compile = do
  handle <- openFile "code.ly" ReadMode
  contents <- hGetContents handle
  let tokens = llex contents
  let st = parse tokens

  let program = case st of
        Left errors -> error $ show errors
        Right (stmts, _) -> generate $ lysseProgram stmts

  B.writeFile "ly" (runPut program)
