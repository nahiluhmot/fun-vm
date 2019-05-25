module Language.VirtualMachine.Data.Stack ( StackConfig(..)
                                          , StackState(..)
                                          , StackFrame(..)
                                          ) where

import Data.IntMap (IntMap)

data StackConfig
  = StackConfig { stMaxDepth :: Int
                , stMain :: Int
                } deriving (Eq, Show)

data StackState
  = StateState { stFrames :: [StackFrame]
               , stDepth :: Int
               } deriving (Eq, Show)

data StackFrame
  = StackFrame { stFuncAddr :: Int
               , stFuncIdx :: Int
               , stScope :: [IntMap Int]
               , stModule :: Int
               } deriving (Eq, Show)
