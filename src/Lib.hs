module Lib
  ( compile,
  )
where

import Data.Binary.Put (runPut)
import Data.ByteString.Lazy as B (writeFile)
import Elf (generateElf)
import LyGen (lysseProgram)
import LyParser (parseLy)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

compile :: IO ()
compile = do
  -- Read
  handle <- openFile "code.ly" ReadMode
  contents <- hGetContents handle

  -- Parse
  let newSt = parseLy contents

  case newSt of
    Left e ->
      do
        putStrLn e
    Right ss ->
      do
        -- Generate
        let program = lysseProgram ss
        let elf = generateElf program
        B.writeFile "ly" (runPut elf)
