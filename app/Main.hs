module Main where

import System.IO (stdin)

import Data.Text (pack)
import Data.Text.IO (hGetContents)

import Language.Fun (compile, runLexer, runParser)

main :: IO ()
main = do
  text <- hGetContents stdin

  let sourceName = "*stdin*"

  case runLexer sourceName text >>= runParser sourceName of
    Left err -> print err
    Right stmts -> mapM_ print $ compile (pack sourceName) stmts
