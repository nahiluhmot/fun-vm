module Main where

import System.IO (stdin)

import Data.Text.IO (hGetContents)

import Language.VirtualMachine (runLexer, runParser)

main :: IO ()
main = do
  text <- hGetContents stdin

  putStrLn "lexer: "
  case runLexer "*stdin*" text of
    Left err -> print err
    Right tokens -> do
      mapM_ print tokens

      putStrLn "parser:"
      case runParser "*stdin*" tokens of
        Left err -> print err
        Right stmts -> mapM_ print stmts
