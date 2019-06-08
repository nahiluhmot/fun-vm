{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Language.Fun.Loader ( load
                           ) where

import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type LoaderT str content
  = StateT (LoaderState str content)

data LoaderState str content =
  LoaderState { loadedModules :: Map str (Set str, content)
              , toLoadModules :: Set str
              } deriving (Eq, Show)

load :: (Ord str, Monad m)
     => str
     -> (str -> m (Set str, content))
     -> m (Map str (Set str, content))
load str loadOne =
  loadedModules <$>
    execStateT (loadAll $ lift . loadOne)
               (initialState str)

initialState :: Ord str => str -> LoaderState str content
initialState key =
  LoaderState M.empty [key]

loadAll :: (Ord str, Monad m)
        => (str -> LoaderT str content m (Set str, content))
        -> LoaderT str content m ()
loadAll loadOne =
  unfoldM nextDependency $ \key ->
    loadOne key >>= insertLoaded key

nextDependency :: (Ord str, Monad m)
               => LoaderT str content m (Maybe str)
nextDependency =
  gets $ S.lookupMin . toLoadModules

insertLoaded :: (Ord str, Monad m)
             => str
             -> (Set str, content)
             -> LoaderT str content m ()
insertLoaded key result@(deps, _content) =
  modify $ \(LoaderState loaded toLoad) ->
    let loaded' =
          M.insert key result loaded

        depsWithLoaded =
          S.union toLoad deps

        toLoad' =
          S.union depsWithLoaded
                  (S.difference depsWithLoaded
                                (M.keysSet loaded'))
    in  LoaderState loaded' toLoad'

unfoldM :: Monad m
      => m (Maybe a)
      -> (a -> m ())
      -> m ()
unfoldM yielder action =
  fix $ \recur ->
    yielder >>= \case
      Just val -> action val >> recur
      Nothing -> return ()
