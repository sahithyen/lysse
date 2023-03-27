module Lib
  ( compile,
  )
where

import Data.Binary.Put (runPut)
import Data.ByteString.Lazy as B (writeFile)
import Elf (generateElf)
import Lexer (llex)
import LyGen (lysseProgram)
import LyParser (parseLy)
import OldParser (parse)
import STree (showStatements)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

compile :: IO ()
compile = do
  handle <- openFile "code.ly" ReadMode
  contents <- hGetContents handle

  putStrLn "-- Old parser"
  let tokens = llex contents
  let st = parse tokens
  either
    ( \e -> do
        putStrLn "No tree to show"
        print e
    )
    ( \(stmts, _) -> do
        putStrLn "Tree:"
        putStrLn $ showStatements stmts
    )
    st

  putStrLn ""

  putStrLn "-- New parser"
  let newSt = parseLy contents
  either
    ( \e -> do
        putStrLn "No tree to show"
        putStrLn e
    )
    ( \stmts -> do
        putStrLn "Tree:"
        putStrLn $ showStatements stmts
    )
    newSt

  putStrLn ""

  case newSt of
    Left e ->
      do
        putStrLn e
    Right ss ->
      do
        let program = lysseProgram ss
        let elf = generateElf program
        B.writeFile "ly" (runPut elf)
