{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Language.VirtualMachine.Data.Fix ( Fix(..)
                                        , cata
                                        , ana
                                        ) where

data Fix f = Fix { unFix :: f (Fix f)  }

cata :: Functor f => (f a -> a) -> Fix f -> a
cata phi = phi . fmap (cata phi) . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana psi = Fix . fmap (ana psi) . psi
