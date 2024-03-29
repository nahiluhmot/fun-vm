module Language.Fun.Instruction ( Instruction(..)
                                ) where

data Instruction sym int val
  = Push val
  | Pop
  | Dup
  | Var sym
  | Let sym
  | BranchIf int
  | Funcall int
  | Lambda int
  | Return
  | Raise
  | PushErrHandler
  | PopErrHandler
  | Def sym
  | Import sym
  | Module sym
  | EndModule sym
  | Debugger
  deriving (Eq, Show)
