module Lib
  ( entry,
  )
where

import Architecture (Architecture (..))
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

data LycArgs = Args {source :: String, output :: String, target :: String, showSt :: Bool}

lycArgsParser :: ParserSpec LycArgs
lycArgsParser =
  Args
    `parsedBy` reqPos "source"
    `Descr` "Path to input Lysse code"
    `andBy` reqPos "output"
    `Descr` "Output path of the generated executable"
    `andBy` reqPos "arch"
    `Descr` "Target architecture (amd64, aarch64)"
    `andBy` boolFlag "show-syntax-tree"
    `Descr` "Prints the syntax tree generated from the input Lysse code"

lycInterface :: IO (CmdLnInterface LycArgs)
lycInterface =
  (`setAppDescr` "Compiles lysse code to a Linux executable")
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

  let ma = case target args of
        "amd64" -> Just Amd64
        "aarch64" -> Just Aarch64
        _ -> Nothing

  case ma of
    Just arch ->
      case st of
        Left e ->
          do
            putStrLn e
        Right ss ->
          do
            -- Generate
            let program = lysseProgram arch ss
            let elf = generateElf arch program
            B.writeFile (output args) (runPut elf)
    Nothing -> putStrLn "Unknown architecture (possible architectures: amd64 / aarch64)"
