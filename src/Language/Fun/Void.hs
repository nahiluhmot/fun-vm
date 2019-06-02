{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}

module Language.Fun.Void ( VoidF
                         , absurdF
                         ) where

-- There are none of a.
data VoidF a deriving (Eq, Show)

absurdF :: VoidF a -> a
absurdF void = case void of {}
