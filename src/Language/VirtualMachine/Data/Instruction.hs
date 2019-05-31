module Language.VirtualMachine.Data.Instruction ( Instruction
                                                , InstructionF(..)
                                                ) where

import Data.Word (Word32)

type Instruction
  = InstructionF Word32

data InstructionF arg
  = Push arg
  | Syscall arg
  | Lookup arg
  | Funcall arg
  | Set arg
  | Return
  | BranchIf arg
  | Recur arg
  | Def arg
  | Import arg
  | Module arg
  | Pop
  | Nop
  deriving (Eq, Show)
