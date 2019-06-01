{-# LANGUAGE TypeFamilies #-}

module Language.Fun.Data.SymbolTable ( SymbolTable
                                     , empty
                                     , size
                                     , toSymbol
                                     , fromSymbol
                                     , garbageCollect
                                     ) where

import GHC.Exts (IsList(..))

import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe

data SymbolTable a = SymbolTable { _idToSym :: (IM.IntMap a)
                                 , _symToID :: (M.Map a Int)
                                 , _nextSym :: Int
                                 , _len :: Int
                                 } deriving (Eq, Show)

empty :: SymbolTable a
empty = SymbolTable IM.empty M.empty 0 0

size :: SymbolTable a -> Int
size = _len

toSymbol :: Ord a => a -> SymbolTable a -> (Int, SymbolTable a)
toSymbol val table@(SymbolTable symToVal valToSym nextSym len) =
  let existing = flip (,) table <$> M.lookup val valToSym
      new = (nextSym, SymbolTable symToVal' valToSym' nextSym' len')
      symToVal' = IM.insert nextSym val symToVal
      valToSym' = M.insert val nextSym valToSym
      nextSym' = succ nextSym
      len' = succ len
  in  fromMaybe new existing

fromSymbol :: Int -> SymbolTable a -> (Maybe a)
fromSymbol sym = IM.lookup sym . _idToSym

garbageCollect :: IS.IntSet -> SymbolTable a -> SymbolTable a
garbageCollect symbols (SymbolTable symToVal valToSym _ _) =
  let symToVal' = IM.restrictKeys symToVal symbols
      valToSym' = M.filter (flip IS.member symbols) valToSym
      nextSym' = succ $ IS.findMax symbols
      len' = IM.size symToVal'
  in  SymbolTable symToVal' valToSym' nextSym' len'

instance Ord a => IsList (SymbolTable a) where
  type Item (SymbolTable a) = (Int, a)

  fromList xs =
    let idToSym = IM.fromList xs
        symToID = foldr (uncurry $ flip M.insert) M.empty xs
        nextSym = succ . fst $ IM.findMax idToSym
        len = IM.size idToSym
    in  SymbolTable idToSym symToID nextSym len

  toList = IM.toList . _idToSym
