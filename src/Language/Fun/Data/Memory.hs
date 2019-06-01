module Language.Fun.Data.Memory ( MemoryConfig(..)
                                , MemoryState(..)
                                ) where

data MemoryConfig int float
  = MemoryConfig { memNurserySize :: int

                 , memHeapStartGC :: int
                 , memHeapGCGrowthFactor :: float
                 , memHeapGCMaxGrowth :: int

                 , memSymbolTableStartGC :: int
                 , memSymbolTableGCGrowthFactor :: float
                 , memSymbolTableGCMaxGrowth :: int

                 } deriving (Eq, Show)

data MemoryState int symbolTable ary
  = MemoryState { memNursery :: ary
                , memHeap :: ary
                , memSymbolTable :: symbolTable
                , memGeneration :: int
                , memNextNurseryKey :: int
                , memNextHeapGC :: int
                } deriving (Eq, Show)
