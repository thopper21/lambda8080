module Processor where

import           Data.Bits
import           Data.IntMap
import           Data.Maybe
import           Data.Word
import           Instruction

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

readRegister8 M processor = readMemory8 (readRegister16 HL processor) processor
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

readRegister16' high low processor = to16 highVal lowVal
  where
    highVal = readRegister8 high processor
    lowVal = readRegister8 low processor

to16 :: Word8 -> Word8 -> Word16
to16 high low = shift (fromIntegral high) 8 .|. fromIntegral low

from16 :: Word16 -> (Word8, Word8)
from16 value = (fromIntegral $ shift value (-8), fromIntegral value)

readMemory8 addr = fromJust . Data.IntMap.lookup (fromIntegral addr) . memory

readMemory16 addr processor = to16 high low
  where
    low = readMemory8 addr processor
    high = readMemory8 (addr + 1) processor

readImmediate8 processor = (result, newProcessor)
  where
    counter = readRegister16 PC processor
    result = readMemory8 counter processor
    newProcessor = writeRegister16 PC (counter + 1) processor

readImmediate16 processor = (to16 high low, newProcessor')
  where
    (low, newProcessor) = readImmediate8 processor
    (high, newProcessor') = readImmediate8 newProcessor

writeRegister8 M value processor =
  writeMemory8 (readRegister16 HL processor) value processor
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
    (highVal, lowVal) = from16 value

writeMemory8 addr value processor =
  processor {memory = insert (fromIntegral addr) value (memory processor)}

writeMemory16 addr value processor = newProcessor'
  where
    (high, low) = from16 value
    newProcessor = writeMemory8 addr low processor
    newProcessor' = writeMemory8 (addr + 1) high newProcessor

processBinaryArithmetic from op processor =
  processor {flags = newFlags, registers = newRegisters}
  where
    left = fromIntegral $ readRegister8 A processor
    right = fromIntegral $ readRegister8 from processor
    result = left `op` right
    newFlags = getArithmeticFlags result
    newRegisters = (registers processor) {a = fromIntegral result}

processImmediateBinaryArithmetic op processor = newProcessor' {flags = newFlags}
  where
    left = readRegister8 A processor
    (right, newProcessor) = readImmediate8 processor
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
process (MOV to from) processor =
  writeRegister8 to (readRegister8 from processor) processor
process (MVI to) processor = writeRegister8 to value newProcessor
  where
    (value, newProcessor) = readImmediate8 processor
process (LXI to) processor = writeRegister16 to addr newProcessor
  where
    (addr, newProcessor) = readImmediate16 processor
process (STAX to) processor = writeMemory8 addr value processor
  where
    addr = readRegister16 to processor
    value = readRegister8 A processor
process (LDAX from) processor = writeRegister8 A value processor
  where
    value = readMemory8 (readRegister16 from processor) processor
process STA processor = writeMemory8 addr value newProcessor
  where
    (addr, newProcessor) = readImmediate16 processor
    value = readRegister8 A newProcessor
process LDA processor = writeRegister8 A value newProcessor
  where
    (addr, newProcessor) = readImmediate16 processor
    value = readMemory8 addr newProcessor
process SHLD processor = writeMemory16 addr value newProcessor
  where
    value = readRegister16 HL processor
    (addr, newProcessor) = readImmediate16 processor
process LHLD processor = writeRegister16 HL value newProcessor
  where
    (addr, newProcessor) = readImmediate16 processor
    value = readMemory16 addr newProcessor
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
process (ADD from) processor = processBinaryArithmetic from (+) processor
process (ADC from) processor =
  processBinaryArithmetic from (withCarry (+) processor) processor
process ADI processor = processImmediateBinaryArithmetic (+) processor
process ACI processor =
  processImmediateBinaryArithmetic (withCarry (+) processor) processor
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
process (SUB from) processor = processBinaryArithmetic from (-) processor
process (SBB from) processor =
  processBinaryArithmetic from (withCarry (-) processor) processor
process SUI processor = processImmediateBinaryArithmetic (-) processor
process SBI processor =
  processImmediateBinaryArithmetic (withCarry (-) processor) processor
