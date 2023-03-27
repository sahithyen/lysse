module Lib
  ( entry,
  )
where

import Control.Monad (when)
import Data.Binary.Put (runPut)
import Data.ByteString.Lazy as B (writeFile)
import Elf (generateElf)
import LyGen (lysseProgram)
import LyParser (parseLy)
import STree (showStatements)
import System.Console.ArgParser
  ( CmdLnInterface,
    Descr (Descr),
    ParserSpec,
    andBy,
    boolFlag,
    mkApp,
    parsedBy,
    reqPos,
    runApp,
    setAppDescr,
  )
import System.IO (IOMode (ReadMode), hGetContents, openFile)

data LycArgs = Args {source :: String, output :: String, showSt :: Bool}

lycArgsParser :: ParserSpec LycArgs
lycArgsParser =
  Args
    `parsedBy` reqPos "source"
    `Descr` "Path to input Lysse code"
    `andBy` reqPos "output"
    `Descr` "Output path of the generated executable"
    `andBy` boolFlag "show-syntax-tree"
    `Descr` "Prints the syntax tree generated from the input Lysse code"

lycInterface :: IO (CmdLnInterface LycArgs)
lycInterface =
  (`setAppDescr` "Compiles lysse code to a Linux executable (aarch64)")
    <$> mkApp lycArgsParser

entry :: IO ()
entry = do
  interface <- lycInterface
  runApp interface compile

compile :: LycArgs -> IO ()
compile args = do
  -- Read
  let sourceFile = source args
  handle <- openFile sourceFile ReadMode
  contents <- hGetContents handle

  -- Parse
  let st = parseLy contents

  when (showSt args) $ case st of
    Right ss -> do
      putStrLn "syntax tree:"
      putStr $ showStatements ss
    _ -> putStrLn "unable to show syntax tree"

  case st of
    Left e ->
      do
        putStrLn e
    Right ss ->
      do
        -- Generate
        let program = lysseProgram ss
        let elf = generateElf program
        B.writeFile (output args) (runPut elf)
