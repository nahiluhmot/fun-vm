{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}

module Language.VirtualMachine.Data.Void ( VoidF
                                         , absurdF
                                         )where

-- There are none of a.
data VoidF a deriving (Eq, Show)

absurdF :: VoidF a -> a
absurdF void = case void of {}
