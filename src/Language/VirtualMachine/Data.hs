module Language.VirtualMachine.Data ( Value
                                    , ValueF(..)
                                    , Function(..)
                                    , Instruction
                                    , InstructionF(..)
                                    , MemoryConfig(..)
                                    , MemoryState(..)
                                    , StackConfig(..)
                                    , StackState(..)
                                    , StackFrame
                                    ) where

import Data.Word (Word32)

import Data.IntMap (IntMap)
import Data.Sequence (Seq)
import Data.Text (Text)

import Language.VirtualMachine.Data.SymbolTable (SymbolTable)

-- Value

type Value
  = ValueF Int Int Double Text Function Seq IntMap Word32

data ValueF sym int float str func vec intMap ref
  = Nil
  | Sym sym
  | Int int
  | Float float
  | Str str
  | Func func
  | Cons ref ref
  | Vector (vec ref)
  | Map (intMap ref)
  deriving (Eq, Show)

instance (Functor vec, Functor intMap) => Functor (ValueF sym int float str func vec intMap) where
  fmap f =
    let go Nil = Nil
        go (Sym s) = Sym s
        go (Int i) = Int i
        go (Float g) = Float g
        go (Str text) = Str text
        go (Cons car cdr) = Cons (f car) (f cdr)
        go (Func func) = Func func
        go (Vector xs) = Vector $ fmap f xs
        go (Map refs) = Map $ fmap f refs
    in  go

-- Function

data Function
  = Function { fnInsns :: Seq Instruction
             , fnArgIDs :: [Int]
             , fnExtraArgID :: Maybe Int
             , fnSource :: Value
             } deriving (Eq, Show)
-- Instruction

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

-- Memory

data MemoryConfig
  = MemoryConfig { memNurserySize :: Int

                 , memHeapStartGC :: Int
                 , memHeapGCGrowthFactor :: Rational
                 , memHeapGCMaxGrowth :: Int

                 , memSymbolTableStartGC :: Int
                 , memSymbolTableGCGrowthFactor :: Rational
                 , memSymbolTableGCMaxGrowth :: Int

                 } deriving (Eq, Show)

data MemoryState
  = MemoryState { memNursery :: IntMap Value
                , memHeap :: IntMap Value
                , memSymbolTable :: SymbolTable Text
                , memNextNurseryKey :: Int
                , memNextHeapGC :: Int
                } deriving (Eq, Show)

-- Stack

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
               , stScope :: [IntMap Word32]
               , stModule :: Int
               } deriving (Eq, Show)
