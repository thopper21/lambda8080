module Processor where

import           Data.Bits
import           Data.IntMap
import           Data.Maybe
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

readRegister8 A processor = a . registers $ processor
readRegister8 B processor = b . registers $ processor
readRegister8 C processor = c . registers $ processor
readRegister8 D processor = d . registers $ processor
readRegister8 E processor = e . registers $ processor
readRegister8 H processor = h . registers $ processor
readRegister8 L processor = l . registers $ processor
readRegister8 M processor = readMemory (readRegister16 HL processor) processor

readRegister16 BC = readRegister16' B C
readRegister16 DE = readRegister16' D E
readRegister16 HL = readRegister16' H L
readRegister16 PC = pc . registers
readRegister16 SP = sp . registers

readRegister16' high low processor =
  shift (fromIntegral highVal) 8 + fromIntegral lowVal
  where
    highVal = readRegister8 high processor
    lowVal = readRegister8 low processor

readMemory :: Word16 -> Processor -> Word8
readMemory addr = fromJust . Data.IntMap.lookup (fromIntegral addr) . memory

writeRegister A value processor =
  processor {registers = (registers processor) {a = value}}
writeRegister B value processor =
  processor {registers = (registers processor) {b = value}}
writeRegister C value processor =
  processor {registers = (registers processor) {c = value}}
writeRegister D value processor =
  processor {registers = (registers processor) {d = value}}
writeRegister E value processor =
  processor {registers = (registers processor) {e = value}}
writeRegister H value processor =
  processor {registers = (registers processor) {h = value}}
writeRegister L value processor =
  processor {registers = (registers processor) {l = value}}
writeRegister M value processor =
  writeMemory (readRegister16 HL processor) value processor

writeMemory addr value processor =
  processor {memory = insert (fromIntegral addr) value (memory processor)}

processBinaryArithmetic ::
     Register8 -> (Word16 -> Word16 -> Word16) -> Processor -> Processor
processBinaryArithmetic from op processor =
  processor {flags = newFlags, registers = newRegisters}
  where
    left = fromIntegral $ readRegister8 A processor
    right = fromIntegral $ readRegister8 from processor
    result = left `op` right
    newFlags = getArithmeticFlags result
    newRegisters = (registers processor) {a = fromIntegral result}

withCarry ::
     (Word16 -> Word16 -> Word16) -> Processor -> Word16 -> Word16 -> Word16
withCarry op processor left right =
  if cy . flags $ processor
    then left `op` right `op` 1
    else left `op` right

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

getIncDecFlags result flags =
  flags
    { z = result == 0
    , s = testBit result 7
    , p = even . popCount $ result .&. 0xff
    }

process :: Instruction -> Processor -> Processor
process NOP processor = processor
process (ADD from) processor = processBinaryArithmetic from (+) processor
process (ADC from) processor =
  processBinaryArithmetic from (withCarry (+) processor) processor
process (SUB from) processor = processBinaryArithmetic from (-) processor
process (SBB from) processor =
  processBinaryArithmetic from (withCarry (-) processor) processor
process (DAD from) processor =
  processor {flags = newFlags, registers = newRegisters}
  where
    left :: Word32
    left = fromIntegral $ readRegister16 HL processor
    right :: Word32
    right = fromIntegral $ readRegister16 from processor
    result = left + right
    newFlags = (flags processor) {cy = result > 0xffff}
    high = shift (result .&. 0xff00) (-8)
    low = result .&. 0xff
    newRegisters =
      (registers processor) {h = fromIntegral high, l = fromIntegral low}
process (INR reg) processor = newProcessor {flags = newFlags}
  where
    newReg = readRegister8 reg processor + 1
    newFlags = getIncDecFlags newReg (flags processor)
    newProcessor = writeRegister reg newReg processor
