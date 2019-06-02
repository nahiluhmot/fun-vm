{-# LANGUAGE TypeFamilies #-}

module Language.Fun.Index ( Index
                          , empty
                          , size
                          , findOrInsert
                          , lookup
                          , garbageCollect
                          , fromList
                          , toList
                          ) where

import Prelude hiding (lookup)
import qualified GHC.Exts as E

import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe

data Index a
  = Index { _byKey :: (IM.IntMap a)
          , _byValue :: (M.Map a Int)
          , _nextID :: Int
          , _len :: Int
          } deriving (Eq, Show)

empty :: Index a
empty =
  Index IM.empty M.empty 0 0

size :: Index a -> Int
size =
  _len

findOrInsert :: Ord a => a -> Index a -> (Int, Index a)
findOrInsert val table@(Index byKey byVal nextID len) =
  let existing = flip (,) table <$> M.lookup val byVal
      new = (nextID, Index byKey' byVal' nextID' len')
      byKey' = IM.insert nextID val byKey
      byVal' = M.insert val nextID byVal
      nextID' = succ nextID
      len' = succ len
  in  fromMaybe new existing

lookup :: Int -> Index a -> Maybe a
lookup key =
  IM.lookup key . _byKey

garbageCollect :: IS.IntSet -> Index a -> Index a
garbageCollect keys (Index byKey byVal _ _) =
  let byKey' = IM.restrictKeys byKey keys
      byVal' = M.filter (flip IS.member keys) byVal
      nextID' = succ $ IS.findMax keys
      len' = IM.size byKey'
  in  Index byKey' byVal' nextID' len'

fromList :: Ord a => [(Int, a)] -> Index a
fromList =
  E.fromList

toList :: Ord a => Index a -> [(Int, a)]
toList =
  E.toList

instance Ord a => E.IsList (Index a) where
  type Item (Index a) = (Int, a)

  fromList xs =
    let byKey = IM.fromList xs
        byValue = foldr (uncurry $ flip M.insert) M.empty xs
        nextID = succ . fst $ IM.findMax byKey
        len = IM.size byKey
    in  Index byKey byValue nextID len

  toList = IM.toList . _byKey
