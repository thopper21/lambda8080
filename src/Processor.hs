module Processor where

import           Control.Monad.State
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

readRegister8 :: Register8 -> State Processor Word8
readRegister8 M = do
  addr <- readRegister16 HL
  readMemory8 addr
readRegister8 reg = gets $ toReader reg . registers

toReader A = a
toReader B = b
toReader C = c
toReader D = d
toReader E = e
toReader H = h
toReader L = l

readRegister16 :: Register16 -> State Processor Word16
readRegister16 BC = readRegister16' B C
readRegister16 DE = readRegister16' D E
readRegister16 HL = readRegister16' H L
readRegister16 PC = gets $ pc . registers
readRegister16 SP = gets $ sp . registers

readRegister16' :: Register8 -> Register8 -> State Processor Word16
readRegister16' high low = do
  highVal <- readRegister8 high
  lowVal <- readRegister8 low
  return $ to16 highVal lowVal

to16 :: Word8 -> Word8 -> Word16
to16 high low = shift (fromIntegral high) 8 .|. fromIntegral low

from16 :: Word16 -> (Word8, Word8)
from16 value = (fromIntegral $ shift value (-8), fromIntegral value)

readMemory8 :: Word16 -> State Processor Word8
readMemory8 addr =
  gets $ fromJust . Data.IntMap.lookup (fromIntegral addr) . memory

readMemory16 :: Word16 -> State Processor Word16
readMemory16 addr = do
  low <- readMemory8 addr
  high <- readMemory8 (addr + 1)
  return $ to16 high low

readImmediate8 :: State Processor Word8
readImmediate8 = do
  counter <- readRegister16 PC
  result <- readMemory8 counter
  writeRegister16 PC (counter + 1)
  return result

readImmediate16 :: State Processor Word16
readImmediate16 = do
  low <- readImmediate8
  high <- readImmediate8
  return $ to16 high low

writeRegister8 :: Register8 -> Word8 -> State Processor ()
writeRegister8 M value = do
  addr <- readRegister16 HL
  writeMemory8 addr value
writeRegister8 reg value =
  modify $ \processor ->
    processor {registers = toWriter reg value (registers processor)}

toWriter A value registers = registers {a = value}
toWriter B value registers = registers {b = value}
toWriter C value registers = registers {c = value}
toWriter D value registers = registers {d = value}
toWriter E value registers = registers {e = value}
toWriter H value registers = registers {h = value}
toWriter L value registers = registers {l = value}

writeRegister16 :: Register16 -> Word16 -> State Processor ()
writeRegister16 BC value = writeRegister16' B C value
writeRegister16 DE value = writeRegister16' D E value
writeRegister16 HL value = writeRegister16' H L value
writeRegister16 PC value =
  modify $ \processor ->
    processor {registers = (registers processor) {pc = value}}
writeRegister16 SP value =
  modify $ \processor ->
    processor {registers = (registers processor) {sp = value}}

writeRegister16' :: Register8 -> Register8 -> Word16 -> State Processor ()
writeRegister16' high low value = do
  let (highVal, lowVal) = from16 value
  writeRegister8 high highVal
  writeRegister8 low lowVal

writeMemory8 :: Word16 -> Word8 -> State Processor ()
writeMemory8 addr value =
  modify $ \processor ->
    processor {memory = insert (fromIntegral addr) value (memory processor)}

writeMemory16 :: Word16 -> Word16 -> State Processor ()
writeMemory16 addr value = do
  let (high, low) = from16 value
  writeMemory8 addr low
  writeMemory8 (addr + 1) high

processRegisterBinaryArithmetic ::
     Register8 -> (Word16 -> Word16 -> Word16) -> State Processor ()
processRegisterBinaryArithmetic = processBinaryArithmetic . readRegister8

processImmediateBinaryArithmetic ::
     (Word16 -> Word16 -> Word16) -> State Processor ()
processImmediateBinaryArithmetic = processBinaryArithmetic readImmediate8

processBinaryArithmetic ::
     State Processor Word8 -> (Word16 -> Word16 -> Word16) -> State Processor ()
processBinaryArithmetic getter op = do
  left <- readRegister8 A
  right <- getter
  let result = fromIntegral left `op` fromIntegral right
  updateArithmeticFlags result
  writeRegister8 A (fromIntegral result)

carry ::
     (Word16 -> Word16 -> Word16)
  -> State Processor (Word16 -> Word16 -> Word16)
carry op = do
  isSet <- gets $ cy . flags
  return $
    if isSet
      then \left right -> left `op` right `op` 1
      else op

setFlags :: (Flags -> Flags) -> State Processor ()
setFlags update =
  modify $ \processor -> processor {flags = update (flags processor)}

-- Force 16 bits here to easily check for the carry flag
updateArithmeticFlags :: Word16 -> State Processor ()
updateArithmeticFlags result =
  setFlags $ \flags ->
    flags
      { z = result .&. 0xff == 0
      , s = testBit result 7
      , p = even . popCount $ result .&. 0xff
      , cy = result > 0xff
      }

updateIncFlags :: Word8 -> State Processor ()
updateIncFlags result =
  setFlags $ \flags ->
    flags
      { z = result == 0
      , s = testBit result 7
      , p = even . popCount $ result .&. 0xff
      }

process :: Instruction -> State Processor ()
process NOP = return ()
process (MOV to from) = do
  value <- readRegister8 from
  writeRegister8 to value
process (MVI to) = do
  value <- readImmediate8
  writeRegister8 to value
process (LXI to) = do
  addr <- readImmediate16
  writeRegister16 to addr
process (STAX to) = do
  addr <- readRegister16 to
  value <- readRegister8 A
  writeMemory8 addr value
process (LDAX from) = do
  addr <- readRegister16 from
  value <- readMemory8 addr
  writeRegister8 A value
process STA = do
  addr <- readImmediate16
  value <- readRegister8 A
  writeMemory8 addr value
process LDA = do
  addr <- readImmediate16
  value <- readMemory8 addr
  writeRegister8 A value
process SHLD = do
  value <- readRegister16 HL
  addr <- readImmediate16
  writeMemory16 addr value
process LHLD = do
  addr <- readImmediate16
  value <- readMemory16 addr
  writeRegister16 HL value
process XCHG = do
  oldDE <- readRegister16 DE
  oldHL <- readRegister16 HL
  writeRegister16 DE oldHL
  writeRegister16 HL oldDE
process (INR reg) = do
  value <- readRegister8 reg
  let newValue = value + 1
  updateIncFlags newValue
  writeRegister8 reg newValue
process (DCR reg) = do
  value <- readRegister8 reg
  let newValue = value - 1
  updateIncFlags newValue
  writeRegister8 reg newValue
process (INX reg) = do
  value <- readRegister16 reg
  let newValue = value + 1
  writeRegister16 reg newValue
process (DCX reg) = do
  value <- readRegister16 reg
  let newValue = value - 1
  writeRegister16 reg newValue
process (ADD from) = processRegisterBinaryArithmetic from (+)
process (ADC from) = do
  op <- carry (+)
  processRegisterBinaryArithmetic from op
process ADI = processImmediateBinaryArithmetic (+)
process ACI = do
  op <- carry (+)
  processImmediateBinaryArithmetic op
process (DAD from) = do
  left <- readRegister16 HL
  right <- readRegister16 from
  let result = fromIntegral left + fromIntegral right :: Word32
  modify $ \processor ->
    processor {flags = (flags processor) {cy = result > 0xffff}}
  writeRegister16 HL (fromIntegral result)
process (SUB from) = processRegisterBinaryArithmetic from (-)
process (SBB from) = do
  op <- carry (-)
  processRegisterBinaryArithmetic from op
process SUI = processImmediateBinaryArithmetic (-)
process SBI = do
  op <- carry (-)
  processImmediateBinaryArithmetic op
