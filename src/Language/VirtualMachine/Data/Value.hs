module Language.VirtualMachine.Data.Value ( Value(..)
                                          , Function(..)
                                          ) where

data Value sym float str func vec intMap ref
  = Nil
  | Sym sym
  | Number float
  | Str str
  | Func func
  | Vector (vec ref)
  | Map (intMap ref)
  deriving (Eq, Show)

instance (Functor vec, Functor intMap) => Functor (Value sym float str func vec intMap) where
  fmap f =
    let go Nil = Nil
        go (Sym s) = Sym s
        go (Number g) = Number g
        go (Str text) = Str text
        go (Func func) = Func func
        go (Vector xs) = Vector $ fmap f xs
        go (Map refs) = Map $ fmap f refs
    in  go

data Function args body
  = Function { fnArgs :: args
             , fnBody :: body
             } deriving (Eq, Show)
