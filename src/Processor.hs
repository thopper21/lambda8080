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

readRegister8 M processor = readMemory (readRegister16 HL processor) processor
readRegister8 reg processor = toReader reg . registers $ processor

toReader A = a
toReader B = b
toReader C = c
toReader D = d
toReader E = e
toReader H = h
toReader L = l

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

readMemory addr = fromJust . Data.IntMap.lookup (fromIntegral addr) . memory

readImmediate processor = (result, newProcessor)
  where
    counter = readRegister16 PC processor
    result = readMemory counter processor
    newProcessor = writeRegister16 PC (counter + 1) processor

writeRegister8 M value processor =
  writeMemory (readRegister16 HL processor) value processor
writeRegister8 reg value processor =
  processor {registers = toWriter reg value (registers processor)}

toWriter A value registers = registers {a = value}
toWriter B value registers = registers {b = value}
toWriter C value registers = registers {c = value}
toWriter D value registers = registers {d = value}
toWriter E value registers = registers {e = value}
toWriter H value registers = registers {h = value}
toWriter L value registers = registers {l = value}

writeRegister16 BC value processor = writeRegister16' B C value processor
writeRegister16 DE value processor = writeRegister16' D E value processor
writeRegister16 HL value processor = writeRegister16' H L value processor
writeRegister16 PC value processor =
  processor {registers = (registers processor) {pc = value}}
writeRegister16 SP value processor =
  processor {registers = (registers processor) {sp = value}}

writeRegister16' high low value =
  writeRegister8 high highVal . writeRegister8 low lowVal
  where
    highVal = fromIntegral $ shift (value .&. 0xff00) (-8)
    lowVal = fromIntegral $ value .&. 0xff

writeMemory addr value processor =
  processor {memory = insert (fromIntegral addr) value (memory processor)}

processBinaryArithmetic from op processor =
  processor {flags = newFlags, registers = newRegisters}
  where
    left = fromIntegral $ readRegister8 A processor
    right = fromIntegral $ readRegister8 from processor
    result = left `op` right
    newFlags = getArithmeticFlags result
    newRegisters = (registers processor) {a = fromIntegral result}

processImmediateBinaryArithmetic op processor = 
    newProcessor' {flags = newFlags }
    where
        left = readRegister8 A processor
        (right, newProcessor) = readImmediate processor
        result = fromIntegral left `op` fromIntegral right
        newFlags = getArithmeticFlags result
        newProcessor' = writeRegister8 A (fromIntegral result) newProcessor

withCarry op processor left right =
  if cy . flags $ processor
    then left `op` right `op` 1
    else left `op` right

-- Force 16 bits here to easily check for the carry flag
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
process ADI processor = processImmediateBinaryArithmetic (+) processor
process ACI processor = processImmediateBinaryArithmetic (withCarry (+) processor) processor
process (SUB from) processor = processBinaryArithmetic from (-) processor
process (SBB from) processor =
  processBinaryArithmetic from (withCarry (-) processor) processor
process SUI processor = processImmediateBinaryArithmetic (-) processor
process SBI processor = processImmediateBinaryArithmetic (withCarry (-) processor) processor
process (DAD from) processor =
  processor {flags = newFlags, registers = newRegisters}
  where
    left = fromIntegral $ readRegister16 HL processor :: Word32
    right = fromIntegral $ readRegister16 from processor :: Word32
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
    newProcessor = writeRegister8 reg newReg processor
process (DCR reg) processor = newProcessor {flags = newFlags}
  where
    newReg = readRegister8 reg processor - 1
    newFlags = getIncDecFlags newReg (flags processor)
    newProcessor = writeRegister8 reg newReg processor
process (INX reg) processor = writeRegister16 reg newReg processor
  where
    newReg = readRegister16 reg processor + 1
process (DCX reg) processor = writeRegister16 reg newReg processor
  where
    newReg = readRegister16 reg processor - 1
