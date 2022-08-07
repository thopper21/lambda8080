module Processor where

import           Data.Bits
import           Data.IntMap
import           Data.Word
import           Instruction
import           Register

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

readRegister A = a
readRegister B = b
readRegister C = c
readRegister D = d
readRegister E = e
readRegister H = h
readRegister L = l

process :: Instruction -> Processor -> Processor
process (ADD from) processor =
  processor {flags = newFlags, registers = newRegisters}
  where
    left = fromIntegral $ a . registers $ processor
    right = fromIntegral $ readRegister from . registers $ processor
    result = left + right
    newFlags = getArithmeticFlags result
    oldRegisters = registers processor
    newRegisters = oldRegisters {a = fromIntegral result}

-- Use 16 bits here to easily check for the carry flag
getArithmeticFlags :: Word16 -> Flags
getArithmeticFlags result =
  Flags
    { z = result .&. 0xff == 0
    , s = testBit result 7
    , p = even . popCount $ result .&. 0xff
    , cy = result > 0xff
    , ac = False
    }
