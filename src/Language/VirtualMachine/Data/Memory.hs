module Language.VirtualMachine.Data.Memory ( MemoryConfig(..)
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

data MemoryState int text intMap symbolTable value
  = MemoryState { memNursery :: intMap value
                , memHeap :: intMap value
                , memSymbolTable :: symbolTable text
                , memGeneration :: int
                , memNextNurseryKey :: int
                , memNextHeapGC :: int
                } deriving (Eq, Show)
