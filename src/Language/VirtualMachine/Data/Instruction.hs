module Language.VirtualMachine.Data.Instruction ( Instruction(..)
                                                ) where

data Instruction arg
  = Syscall arg
  | Push arg
  | Lookup arg
  | Funcall arg
  | Set arg
  | Return
  | BranchIf arg
  | Recur arg
  | Def arg
  | Module arg
  | Pop
  | Nop
