module Language.VirtualMachine.Data.Instruction ( Instruction(..)
                                                ) where

data Instruction push funcall branchIf imp def local mod mkFunc
  = Push push
  | Funcall funcall
  | BranchIf branchIf
  | Import imp
  | Def def
  | Local local
  | Module mod
  | MkFunc mkFunc
  deriving (Eq, Show)
