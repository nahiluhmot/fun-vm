module Language.VirtualMachine.Data.Memory ( MemoryConfig(..)
                                           , MemoryState(..)
                                           ) where

import Data.Text (Text)
import Data.IntMap (IntMap)

import Language.VirtualMachine.Data.SymbolTable (SymbolTable)
import Language.VirtualMachine.Data.Value (Value)

data MemoryConfig
  = MemoryConfig { memNurserySize :: Int

                 , memHeapStartGC :: Int
                 , memHeapGCGrowthFactor :: Rational
                 , memHeapGCMaxGrowth :: Int

                 , memSymbolTableStartGC :: Int
                 , memSymbolTableGCGrowthFactor :: Rational
                 , memSymbolTableGCMaxGrowth :: Int

                 } deriving (Eq, Show)

data MemoryState
  = MemoryState { memNursery :: IntMap Value
                , memHeap :: IntMap Value
                , memSymbolTable :: SymbolTable Text
                , memGeneration :: Int
                , memNextNurseryKey :: Int
                , memNextHeapGC :: Int
                } deriving (Eq, Show)
