module Language.VirtualMachine.Data.Instruction ( Instruction(..)
                                                ) where

data Instruction arg
  = Nop
  | Push arg
  | Pop
  | Module arg
  | Lookup arg
  | Set arg
  | Def arg
  | Syscall arg
  | Funcall arg
  | Return
  | Recur arg
  | BranchIf arg
