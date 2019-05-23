module Language.VirtualMachine.Data.Value ( ValueF(..)
                                          ) where

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
