module Main where

import System.IO (stdin)

import Data.Text.IO (hGetContents)

import Language.VirtualMachine (runLexer)

main :: IO ()
main = runLexer "*stdin*" <$> hGetContents stdin >>= print
