module Main where

import System.IO (stdin)

import Data.Text.IO (hGetContents)

import Language.VirtualMachine (runLexer, runParser)

main :: IO ()
main = do
  text <- hGetContents stdin

  let sourceName = "*stdin*"
      lexed = runLexer sourceName text

  print lexed
  print (lexed >>= runParser sourceName)
