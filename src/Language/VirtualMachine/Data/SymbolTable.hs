module Language.VirtualMachine.Data.SymbolTable ( SymbolTable
                                                , empty
                                                , size
                                                , toSymbol
                                                , fromSymbol
                                                , garbageCollect
                                                ) where

import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe

data SymbolTable a = SymbolTable { _idToSym :: (IM.IntMap a)
                                 , _symToID :: (M.Map a Int)
                                 , _nextSym :: Int
                                 , _len :: Int
                                 }

empty :: SymbolTable a
empty = SymbolTable IM.empty M.empty minBound 0

size :: SymbolTable a -> Int
size (SymbolTable _ _ _ len) = len

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
fromSymbol sym (SymbolTable symToVal _ _ _) = IM.lookup sym symToVal

garbageCollect :: IS.IntSet -> SymbolTable a -> SymbolTable a
garbageCollect symbols (SymbolTable symToVal valToSym _ _) =
  let symToVal' = IM.restrictKeys symToVal symbols
      valToSym' = M.filter (flip IS.member symbols) valToSym
      nextSym' = succ $ IS.findMax symbols
      len' = IM.size symToVal'
  in  SymbolTable symToVal' valToSym' nextSym' len'
