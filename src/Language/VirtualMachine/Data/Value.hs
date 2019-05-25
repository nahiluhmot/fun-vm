{-# LANGUAGE TypeFamilies #-}

module Language.VirtualMachine.Data.Value ( ValueF(..)
                                          , FunctionF(..)
                                          ) where

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

data FunctionF sym vec insn
  = FunctionF { fnInsns :: vec insn
              , fnArgs :: vec sym
              , fnExtraArg :: Maybe sym
              } deriving (Eq, Show)
