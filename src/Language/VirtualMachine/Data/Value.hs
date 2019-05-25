module Language.VirtualMachine.Data.Value ( Value
                                          , ValueF(..)
                                          , Function
                                          , FunctionF(..)
                                          ) where

import Data.Sequence (Seq)
import Data.Text (Text)
import Data.IntMap (IntMap)

import Language.VirtualMachine.Data.Instruction (Instruction)

type Value
  = ValueF Int Int Double Text Function Seq IntMap Int

type Function
  = FunctionF Int Seq Instruction Int

data ValueF sym int float str func vec intMap ref
  = Nil
  | Sym sym
  | Int int
  | Float float
  | Str str
  | Func func
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
        go (Func func) = Func func
        go (Vector xs) = Vector $ fmap f xs
        go (Map refs) = Map $ fmap f refs
    in  go

data FunctionF int vec insn ref
  = FunctionF { fnInsns :: vec insn
              , fnArgIDs :: vec int
              , fnExtraArgID :: Maybe int
              , fnSource :: ref
              } deriving (Eq, Show)
