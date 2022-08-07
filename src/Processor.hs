module Processor where

import           Data.IntMap.Lazy
import           Data.Word

data Flags = Flags
  { z  :: Bool
  , s  :: Bool
  , p  :: Bool
  , cy :: Bool
  , ac :: Bool
  }

data Registers = Registers
  { a  :: Word8
  , b  :: Word8
  , c  :: Word8
  , d  :: Word8
  , e  :: Word8
  , h  :: Word8
  , l  :: Word8
  , sp :: Word16
  , pc :: Word16
  }

data Processor = Processor
  { flags     :: Flags
  , registers :: Registers
  , memory    :: IntMap Word8
  }
