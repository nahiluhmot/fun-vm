module Language.Fun.VirtualMachine (
                                   )where

import Data.Word (Word32)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T

import Language.Fun.Compiler (CompilerInsn, CompilerValue)
import Language.Fun.Data.Instruction (Instruction(..))
import Language.Fun.Value (Value(..))
import Language.Fun.Index (Index)
import qualified Language.Fun.Index as IX

type VMInstruction = Instruction Int Int Int
type VMValue = Value Int Rational Text (VMFun VMNativeFun VMFunPtr) Seq IntMap VMRef

data VMFun native ptr
  = Native native
  | FunPtr ptr
  deriving (Eq, Show)

data VMNativeFun
  = VMNativeFun { fName :: Text
                , fRun :: [VMRef] -> VMRef
                }

data VMFunPtr
   = VMFunPtr { fptrStart :: Int
              , fptrLen :: Int
              , fptrBindings :: VMRef
              } deriving (Eq, Show)

newtype VMRef
  = VMRef { runVMRef :: Word32 }
    deriving (Eq, Ord, Show)

data VMConfig
  = VMConfig { nurserySize :: Int
             , heapStartGC :: Int
             , heapGCGrowthFactor :: Rational
             , heapGCMaxGrowth :: Int
             } deriving (Eq, Show)

data VMState
  = VMState { modules :: IntMap VMRef
            , wipModules :: IntMap VMRef

            , instructions :: Seq VMRef
            , programCounter :: VMRef

            , symbolTable :: Index Text

            , errHandlers :: [VMFunPtr]

            , stack :: [VMRef]

            , nextNuresryKey :: Int
            , nextHeapKey :: Int

            , nextHeapGC :: Int

            , nursery :: IntMap VMValue
            , heap :: IntMap VMValue
            } deriving (Eq, Show)

instance Eq VMNativeFun where
  (VMNativeFun name _) == (VMNativeFun name' _) = name == name'

instance Show VMNativeFun where
  show (VMNativeFun name _) = show name
