module Language.Fun.Data.Stack ( StackConfig(..)
                               , StackState(..)
                               , StackFrame(..)
                               ) where

data StackConfig int ref
  = StackConfig { stMaxDepth :: int
                , stMain :: ref
                } deriving (Eq, Show)

data StackState int stackFrame
  = StateState { stFrames :: [stackFrame]
               , stDepth :: int
               } deriving (Eq, Show)

data StackFrame intMap ref
  = StackFrame { stFuncAddr :: ref
               , stFuncIdx :: ref
               , stScope :: [intMap ref]
               , stModule :: ref
               } deriving (Eq, Show)
