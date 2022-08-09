module Processor where

import           Control.Monad.State
import           Data.Bits
import           Data.IntMap
import           Data.Maybe
import           Data.Word
import           Instruction

data Flag
  = Z
  | S
  | P
  | CY
  | AC

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
  { flags     :: Word8
  , registers :: Registers
  , memory    :: IntMap Word8
  }

flagBit :: Flag -> Int
flagBit Z  = 6
flagBit S  = 7
flagBit P  = 2
flagBit CY = 0
flagBit AC = 4

getFlag :: Flag -> State Processor Bool
getFlag flag = do
  flags <- gets flags
  return $ testBit flags (flagBit flag)

assignBit :: Bool -> Word8 -> Int -> Word8
assignBit flag =
  if flag
    then setBit
    else clearBit

setFlag :: Flag -> Bool -> State Processor ()
setFlag flag cond = do
  flags <- gets flags
  let newFlags = assignBit cond flags (flagBit flag)
  modify $ \processor -> processor {flags = newFlags}

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
readRegister16 BC  = readRegister16' B C
readRegister16 DE  = readRegister16' D E
readRegister16 HL  = readRegister16' H L
readRegister16 PC  = gets $ pc . registers
readRegister16 SP  = gets $ sp . registers
readRegister16 PSW = liftM2 to16 (readRegister8 A) (gets flags)

readRegister16' :: Register8 -> Register8 -> State Processor Word16
readRegister16' high low = liftM2 to16 (readRegister8 high) (readRegister8 low)

to16 :: Word8 -> Word8 -> Word16
to16 high low = shift (fromIntegral high) 8 .|. fromIntegral low

from16 :: Word16 -> (Word8, Word8)
from16 value = (fromIntegral $ shift value (-8), fromIntegral value)

readMemory8 :: Word16 -> State Processor Word8
readMemory8 addr =
  gets $ fromJust . Data.IntMap.lookup (fromIntegral addr) . memory

readMemory16 :: Word16 -> State Processor Word16
readMemory16 addr = liftM2 to16 (readMemory8 addr) (readMemory8 (addr + 1))

readImmediate8 :: State Processor Word8
readImmediate8 = do
  counter <- readRegister16 PC
  result <- readMemory8 counter
  writeRegister16 PC (counter + 1)
  return result

readImmediate16 :: State Processor Word16
readImmediate16 = liftM2 to16 readImmediate8 readImmediate8

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
writeRegister16 PSW value = do
  let (high, low) = from16 value
  writeRegister8 A high
  modify $ \processor -> processor {flags = low}

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

registerBinaryArithmetic ::
     Register8 -> (Word16 -> Word16 -> Word16) -> State Processor ()
registerBinaryArithmetic = binaryArithmetic . readRegister8

immediateBinaryArithmetic :: (Word16 -> Word16 -> Word16) -> State Processor ()
immediateBinaryArithmetic = binaryArithmetic readImmediate8

binaryArithmetic ::
     State Processor Word8 -> (Word16 -> Word16 -> Word16) -> State Processor ()
binaryArithmetic getter op = do
  result <- arithmetic op (readRegister8 A) getter
  writeRegister8 A (fromIntegral result)

carry ::
     (Word16 -> Word16 -> Word16)
  -> State Processor (Word16 -> Word16 -> Word16)
carry op = do
  isSet <- getFlag CY
  return $
    if isSet
      then \left right -> left `op` right `op` 1
      else op

testZero :: Word8 -> Bool
testZero = (==) 0

testSign :: Word8 -> Bool
testSign = flip testBit 7

testParity :: Word8 -> Bool
testParity = even . popCount

updateFlags :: Word8 -> State Processor ()
updateFlags result = do
  setFlag Z (testZero result)
  setFlag S (testSign result)
  setFlag P (testParity result)

arithmetic ::
     (Word16 -> Word16 -> Word16)
  -> State Processor Word8
  -> State Processor Word8
  -> State Processor Word8
arithmetic op left right = do
  leftVal <- left
  rightVal <- right
  let result16 = fromIntegral leftVal `op` fromIntegral rightVal
  let result8 = fromIntegral result16
  updateFlags result8
  -- 8 bit arithmetic carry
  setFlag CY (result16 > 0xff)
  -- 4 bit arithmetic carry
  let to4 val = fromIntegral val .&. 0xf
  let left4 = to4 leftVal
  let right4 = to4 rightVal
  setFlag AC ((left4 `op` right4) > 0xf)
  return result8

updateLogicalFlags :: Word8 -> State Processor ()
updateLogicalFlags result = do
  updateFlags result
  setFlag CY False
  setFlag AC False

doIf :: State Processor () -> State Processor Bool -> State Processor ()
doIf action cond = do
  isSet <- cond
  when isSet action

jump :: State Processor ()
jump = readImmediate16 >>= writeRegister16 PC

jumpIf :: Flag -> State Processor ()
jumpIf flag = doIf jump $ getFlag flag

jumpIfNot :: Flag -> State Processor ()
jumpIfNot flag = doIf jump $ not <$> getFlag flag

push :: Word16 -> State Processor ()
push value = do
  currentStack <- readRegister16 SP
  let newStack = currentStack - 2
  writeMemory16 newStack value
  writeRegister16 SP newStack

callAt :: Word16 -> State Processor ()
callAt newCounter = do
  currentCounter <- readRegister16 PC
  push currentCounter
  writeRegister16 PC newCounter

call :: State Processor ()
call = readImmediate16 >>= callAt

callIf :: Flag -> State Processor ()
callIf flag = doIf call $ getFlag flag

callIfNot :: Flag -> State Processor ()
callIfNot flag = doIf call $ not <$> getFlag flag

pop :: State Processor Word16
pop = do
  stack <- readRegister16 SP
  value <- readMemory16 stack
  writeRegister16 SP (stack + 2)
  return value

ret :: State Processor ()
ret = pop >>= writeRegister16 PC

retIf :: Flag -> State Processor ()
retIf flag = doIf ret $ getFlag flag

retIfNot :: Flag -> State Processor ()
retIfNot flag = doIf ret $ not <$> getFlag flag

logical ::
     State Processor Word8 -> (Word8 -> Word8 -> Word8) -> State Processor ()
logical getter op = do
  left <- readRegister8 A
  right <- getter
  let result = left `op` right
  updateLogicalFlags result
  writeRegister8 A result

logicalRegister :: Register8 -> (Word8 -> Word8 -> Word8) -> State Processor ()
logicalRegister = logical . readRegister8

logicalImmediate :: (Word8 -> Word8 -> Word8) -> State Processor ()
logicalImmediate = logical readImmediate8

cmp :: State Processor Word8 -> State Processor ()
cmp getter = do
  arithmetic (-) (readRegister8 A) getter
  return ()

rot :: Int -> (Word8 -> Int -> Word8) -> State Processor ()
rot carryBit op = do
  input <- readRegister8 A
  setFlag CY (testBit input carryBit)
  writeRegister8 A $ op input 1

rotCarry :: Int -> Int -> (Word8 -> Int -> Word8) -> State Processor ()
rotCarry carryBit uncarryBit op = do
  input <- readRegister8 A
  oldCarry <- getFlag CY
  let result = assignBit oldCarry (op input 1) uncarryBit
  setFlag CY (testBit input carryBit)
  writeRegister8 A result

process :: Instruction -> State Processor ()
process NOP = return ()
process (MOV to from) = readRegister8 from >>= writeRegister8 to
process (MVI to) = readImmediate8 >>= writeRegister8 to
process (LXI to) = readImmediate16 >>= writeRegister16 to
process (STAX to) = do
  addr <- readRegister16 to
  value <- readRegister8 A
  writeMemory8 addr value
process (LDAX from) = readRegister16 from >>= readMemory8 >>= writeRegister8 A
process STA = do
  addr <- readImmediate16
  value <- readRegister8 A
  writeMemory8 addr value
process LDA = readImmediate16 >>= readMemory8 >>= writeRegister8 A
process SHLD = do
  value <- readRegister16 HL
  addr <- readImmediate16
  writeMemory16 addr value
process LHLD = readImmediate16 >>= readMemory16 >>= writeRegister16 HL
process XCHG = do
  oldDE <- readRegister16 DE
  oldHL <- readRegister16 HL
  writeRegister16 DE oldHL
  writeRegister16 HL oldDE
process (PUSH from) = readRegister16 from >>= push
process (POP to) = pop >>= writeRegister16 to
process XTHL = do
  stack <- readRegister16 SP
  stackVal <- readMemory16 stack
  hlVal <- readRegister16 HL
  writeRegister16 HL stackVal
  writeMemory16 stack hlVal
process JMP = jump
process JC = jumpIf CY
process JNC = jumpIfNot CY
process JZ = jumpIf Z
process JNZ = jumpIfNot Z
process JP = jumpIfNot S
process JM = jumpIf S
process JPE = jumpIf P
process JPO = jumpIfNot P
process PCHL = readRegister16 HL >>= writeRegister16 PC
process CALL = call
process CC = callIf CY
process CNC = callIfNot CY
process CZ = callIf Z
process CNZ = callIfNot Z
process CP = callIfNot S
process CM = callIf S
process CPE = callIf P
process CPO = callIfNot P
process RET = ret
process RC = retIf CY
process RNC = retIfNot CY
process RZ = retIf Z
process RNZ = retIfNot Z
process RP = retIfNot S
process RM = retIf S
process RPE = retIf P
process RPO = retIfNot P
process (RST exp) = callAt $ shift (fromIntegral exp) 3
process (INR reg) = do
  value <- readRegister8 reg
  let newValue = value + 1
  updateFlags newValue
  setFlag AC (value .&. 0xff == 0xff)
  writeRegister8 reg newValue
process (DCR reg) = do
  value <- readRegister8 reg
  let newValue = value - 1
  updateFlags newValue
  setFlag AC (newValue .&. 0xff == 0xff)
  writeRegister8 reg newValue
process (INX reg) = do
  value <- readRegister16 reg
  let newValue = value + 1
  writeRegister16 reg newValue
process (DCX reg) = do
  value <- readRegister16 reg
  let newValue = value - 1
  writeRegister16 reg newValue
process (ADD from) = registerBinaryArithmetic from (+)
process (ADC from) = carry (+) >>= registerBinaryArithmetic from
process ADI = immediateBinaryArithmetic (+)
process ACI = carry (+) >>= immediateBinaryArithmetic
process (DAD from) = do
  left <- readRegister16 HL
  right <- readRegister16 from
  let result = fromIntegral left + fromIntegral right :: Word32
  setFlag CY (result > 0xffff)
  writeRegister16 HL (fromIntegral result)
process (SUB from) = registerBinaryArithmetic from (-)
process (SBB from) = carry (-) >>= registerBinaryArithmetic from
process SUI = immediateBinaryArithmetic (-)
process SBI = carry (-) >>= immediateBinaryArithmetic
process (ANA from) = logicalRegister from (.&.)
process (XRA from) = logicalRegister from xor
process (ORA from) = logicalRegister from (.|.)
process (CMP from) = cmp $ readRegister8 from
process ANI = logicalImmediate (.&.)
process XRI = logicalImmediate xor
process ORI = logicalImmediate (.|.)
process CPI = cmp readImmediate8
process RLC = rot 7 rotateL
process RRC = rot 0 rotateR
process RAL = rotCarry 7 0 shiftL
process RAR = rotCarry 0 7 shiftR
process CMA = (complement <$> readRegister8 A) >>= writeRegister8 A
process STC = setFlag CY True
process CMC = do
  carry <- getFlag CY
  setFlag CY $ not carry
process DAA = do
  value <- readRegister8 A
  ac <- getFlag AC
  cy <- getFlag CY
  let lowAddend =
        if ac || (value .&. 0xf > 9)
          then 6
          else 0
  let highAddend =
        if cy || (shiftR (value .&. 0xf0) 4 > 9)
          then shiftL 6 4
          else 0
  writeRegister8 A (value + lowAddend + highAddend)
