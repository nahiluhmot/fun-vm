module Language.Fun.VirtualMachine (
                                   ) where

import Data.Word (Word32)
import System.IO (FilePath)

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, runStateT)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T

import Language.Fun.Compiler (CompilerInsn, CompilerValue)
import Language.Fun.Instruction (Instruction(..))
import Language.Fun.Index (Index)
import qualified Language.Fun.Index as IX
import Language.Fun.Value (Value(..))

type VMInstruction = Instruction Int Int Int
type VMValue = Value Int Rational Text (Either VMNativeFun VMFunPtr) Seq IntMap VMRef
type VMFun = Either VMNativeFun VMFunPtr

type MonadVM = ExceptT VMErr (ReaderT VMConfig (StateT VMState IO))

vmRun :: MonadVM a -> VMConfig -> VMState -> IO (Either VMErr a, VMState)
vmRun vm conf state =
  runStateT (runReaderT (runExceptT vm) conf) state

vmInitialState :: VMConfig -> VMState
vmInitialState conf =
  VMState { modules = IM.empty
          , wipModules = IM.empty

          , instructions = S.empty
          , programCounter = VMRef 0

          , index = IX.empty

          , errHandlers = []

          , stack = []

          , nextNurseryKey = 1
          , nextHeapKey = succ $ nurserySize conf

          , nextHeapGC = heapStartGC conf

          , nursery = IM.empty
          , heap = IM.empty

          }

data VMErr
  = VMRuntimeError VMValue
  deriving (Eq, Show)

data VMNativeFun
  = VMNativeFun { fName :: Text
                , fRun :: [VMRef] -> MonadVM VMRef
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

            , index :: Index Text

            , errHandlers :: [VMFunPtr]

            , stack :: [VMRef]

            , nextNurseryKey :: Int
            , nextHeapKey :: Int

            , nextHeapGC :: Int

            , nursery :: IntMap VMValue
            , heap :: IntMap VMValue
            } deriving (Eq, Show)

instance Eq VMNativeFun where
  (VMNativeFun name _) == (VMNativeFun name' _) = name == name'

instance Show VMNativeFun where
  show (VMNativeFun name _) = show name
