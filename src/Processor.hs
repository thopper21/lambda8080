{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Processor
  ( Processor
  , Platform(..)
  , step
  , initProcessor
  ) where

import           Control.Monad.State
import           Data.Bits
import           Data.Word
import           Instruction
import           Numeric

data Flag
  = Z
  | S
  | P
  | CY
  | AC

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

class Platform a where
  readMemory :: Word16 -> a -> Word8
  writeMemory :: Word16 -> Word8 -> a -> a
  readIn :: Word8 -> a -> IO Word8
  writeOut :: Word8 -> Word8 -> a -> IO ()

data Platform a =>
     Processor a = Processor
  { flags             :: Word8
  , registers         :: Registers
  , interruptsEnabled :: Bool
  , halted            :: Bool
  , platform          :: a
  }

instance Platform a => Show (Processor a) where
  show processor =
    "af: " ++
    showReg a ++
    showFlags ++
    " bc: " ++
    showReg b ++
    showReg c ++
    " de: " ++
    showReg d ++
    showReg e ++
    " hl: " ++
    showReg h ++ showReg l ++ " pc: " ++ showReg16 pc ++ " sp:" ++ showReg16 sp
    where
      showReg reg = toStr $ fromIntegral (reg . registers $ processor)
      showReg16 reg = show $ showHex (reg . registers $ processor) ""
      showFlags = toStr $ fromIntegral (flags processor)
      toStr :: Word8 -> String
      toStr x =
        case toStr' x of
          [l, h] -> [h, l]
          [l]    -> ['0', l]
          []     -> ['0', '0']
      toStr' :: Word8 -> String
      toStr' 0 = []
      toStr' x = c : toStr' rem
        where
          c =
            case x `mod` 16 of
              0  -> '0'
              1  -> '1'
              2  -> '2'
              3  -> '3'
              4  -> '4'
              5  -> '5'
              6  -> '6'
              7  -> '7'
              8  -> '8'
              9  -> '9'
              10 -> 'a'
              11 -> 'b'
              12 -> 'c'
              13 -> 'd'
              14 -> 'e'
              15 -> 'f'
          rem = x `div` 16

type ProcessorState a = StateT (Processor a) IO

setInterruptsEnabled :: Platform a => Bool -> ProcessorState a ()
setInterruptsEnabled enabled =
  modify $ \processor -> processor {interruptsEnabled = enabled}

flagBit :: Flag -> Int
flagBit Z  = 6
flagBit S  = 7
flagBit P  = 2
flagBit CY = 0
flagBit AC = 4

getFlag :: Platform a => Flag -> ProcessorState a Bool
getFlag flag = do
  flags <- gets flags
  return $ testBit flags (flagBit flag)

assignBit :: Bool -> Word8 -> Int -> Word8
assignBit flag =
  if flag
    then setBit
    else clearBit

setFlag :: Platform a => Flag -> Bool -> ProcessorState a ()
setFlag flag cond = do
  flags <- gets flags
  let newFlags = assignBit cond flags (flagBit flag)
  modify $ \processor -> processor {flags = newFlags}

readRegister8 :: Platform a => Register8 -> ProcessorState a Word8
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

readRegister16 :: Platform a => Register16 -> ProcessorState a Word16
readRegister16 BC  = readRegister16' B C
readRegister16 DE  = readRegister16' D E
readRegister16 HL  = readRegister16' H L
readRegister16 PC  = gets $ pc . registers
readRegister16 SP  = gets $ sp . registers
readRegister16 PSW = liftM2 to16 (readRegister8 A) (gets flags)

readRegister16' ::
     Platform a => Register8 -> Register8 -> ProcessorState a Word16
readRegister16' high low = liftM2 to16 (readRegister8 high) (readRegister8 low)

to16 :: Word8 -> Word8 -> Word16
to16 high low = shift (fromIntegral high) 8 .|. fromIntegral low

from16 :: Word16 -> (Word8, Word8)
from16 value = (fromIntegral $ shift value (-8), fromIntegral value)

readMemory8 :: Platform a => Word16 -> ProcessorState a Word8
readMemory8 addr = gets $ \processor -> readMemory addr (platform processor)

readMemory16 :: Platform a => Word16 -> ProcessorState a Word16
readMemory16 addr = liftM2 to16 (readMemory8 (addr + 1)) (readMemory8 addr)

readImmediate8 :: Platform a => ProcessorState a Word8
readImmediate8 = do
  counter <- readRegister16 PC
  result <- readMemory8 counter
  writeRegister16 PC (counter + 1)
  return result

readImmediate16 :: Platform a => ProcessorState a Word16
readImmediate16 = liftM2 (flip to16) readImmediate8 readImmediate8

writeRegister8 :: Platform a => Register8 -> Word8 -> ProcessorState a ()
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

writeRegister16 :: Platform a => Register16 -> Word16 -> ProcessorState a ()
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

writeRegister16' ::
     Platform a => Register8 -> Register8 -> Word16 -> ProcessorState a ()
writeRegister16' high low value = do
  let (highVal, lowVal) = from16 value
  writeRegister8 high highVal
  writeRegister8 low lowVal

writeMemory8 :: Platform a => Word16 -> Word8 -> ProcessorState a ()
writeMemory8 addr value =
  modify $ \processor ->
    processor {platform = writeMemory addr value (platform processor)}

writeMemory16 :: Platform a => Word16 -> Word16 -> ProcessorState a ()
writeMemory16 addr value = do
  let (high, low) = from16 value
  writeMemory8 addr low
  writeMemory8 (addr + 1) high

registerBinaryArithmetic ::
     Platform a
  => Register8
  -> (Word16 -> Word16 -> Word16)
  -> ProcessorState a ()
registerBinaryArithmetic = binaryArithmetic . readRegister8

immediateBinaryArithmetic ::
     Platform a => (Word16 -> Word16 -> Word16) -> ProcessorState a ()
immediateBinaryArithmetic = binaryArithmetic readImmediate8

binaryArithmetic ::
     Platform a
  => ProcessorState a Word8
  -> (Word16 -> Word16 -> Word16)
  -> ProcessorState a ()
binaryArithmetic getter op = do
  result <- arithmetic op (readRegister8 A) getter
  writeRegister8 A (fromIntegral result)

carry ::
     Platform a
  => (Word16 -> Word16 -> Word16)
  -> ProcessorState a (Word16 -> Word16 -> Word16)
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

updateFlags :: Platform a => Word8 -> ProcessorState a ()
updateFlags result = do
  setFlag Z (testZero result)
  setFlag S (testSign result)
  setFlag P (testParity result)

arithmetic ::
     Platform a
  => (Word16 -> Word16 -> Word16)
  -> ProcessorState a Word8
  -> ProcessorState a Word8
  -> ProcessorState a Word8
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

updateLogicalFlags :: Platform a => Word8 -> ProcessorState a ()
updateLogicalFlags result = do
  updateFlags result
  setFlag CY False
  setFlag AC False

jump :: Platform a => ProcessorState a ()
jump = readImmediate16 >>= writeRegister16 PC

jumpIf :: Platform a => Flag -> ProcessorState a ()
jumpIf flag = do
  addr <- readImmediate16
  isSet <- getFlag flag
  when isSet $ writeRegister16 PC addr

jumpIfNot :: Platform a => Flag -> ProcessorState a ()
jumpIfNot flag = do
  addr <- readImmediate16
  isSet <- getFlag flag
  unless isSet $ writeRegister16 PC addr

push :: Platform a => Word16 -> ProcessorState a ()
push value = do
  currentStack <- readRegister16 SP
  let newStack = currentStack - 2
  writeMemory16 newStack value
  writeRegister16 SP newStack

callAt :: Platform a => Word16 -> ProcessorState a ()
callAt newCounter = do
  currentCounter <- readRegister16 PC
  push currentCounter
  writeRegister16 PC newCounter

call :: Platform a => ProcessorState a ()
call = readImmediate16 >>= callAt

callIf :: Platform a => Flag -> ProcessorState a ()
callIf flag = do
  addr <- readImmediate16
  isSet <- getFlag flag
  when isSet $ callAt addr

callIfNot :: Platform a => Flag -> ProcessorState a ()
callIfNot flag = do
  addr <- readImmediate16
  isSet <- getFlag flag
  unless isSet $ callAt addr

pop :: Platform a => ProcessorState a Word16
pop = do
  stack <- readRegister16 SP
  value <- readMemory16 stack
  writeRegister16 SP (stack + 2)
  return value

ret :: Platform a => ProcessorState a ()
ret = pop >>= writeRegister16 PC

retIf :: Platform a => Flag -> ProcessorState a ()
retIf flag = do
  isSet <- getFlag flag
  when isSet ret

retIfNot :: Platform a => Flag -> ProcessorState a ()
retIfNot flag = do
  isSet <- getFlag flag
  unless isSet ret

logical ::
     Platform a
  => ProcessorState a Word8
  -> (Word8 -> Word8 -> Word8)
  -> ProcessorState a ()
logical getter op = do
  left <- readRegister8 A
  right <- getter
  let result = left `op` right
  updateLogicalFlags result
  writeRegister8 A result

logicalRegister ::
     Platform a => Register8 -> (Word8 -> Word8 -> Word8) -> ProcessorState a ()
logicalRegister = logical . readRegister8

logicalImmediate ::
     Platform a => (Word8 -> Word8 -> Word8) -> ProcessorState a ()
logicalImmediate = logical readImmediate8

cmp :: Platform a => ProcessorState a Word8 -> ProcessorState a ()
cmp getter = do
  arithmetic (-) (readRegister8 A) getter
  return ()

rot :: Platform a => Int -> (Word8 -> Int -> Word8) -> ProcessorState a ()
rot carryBit op = do
  input <- readRegister8 A
  setFlag CY (testBit input carryBit)
  writeRegister8 A $ op input 1

rotCarry ::
     Platform a => Int -> Int -> (Word8 -> Int -> Word8) -> ProcessorState a ()
rotCarry carryBit uncarryBit op = do
  input <- readRegister8 A
  oldCarry <- getFlag CY
  let result = assignBit oldCarry (op input 1) uncarryBit
  setFlag CY (testBit input carryBit)
  writeRegister8 A result

process :: Platform a => Instruction -> ProcessorState a ()
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
process SPHL = readRegister16 HL >>= writeRegister16 SP
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
process CMA = readRegister8 A >>= writeRegister8 A . complement
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
process IN = do
  port <- readImmediate8
  platform <- gets platform
  result <- liftIO $ readIn port platform
  writeRegister8 A result
process OUT = do
  port <- readImmediate8
  platform <- gets platform
  value <- readRegister8 A
  liftIO $ writeOut port value platform
process EI = setInterruptsEnabled True
process DI = setInterruptsEnabled False
process NOP = return ()
process HLT = modify $ \processor -> processor {halted = True}

step :: Platform a => ProcessorState a Word8
step = do
  opCode <- readImmediate8
  process $ toInstruction opCode
  return opCode

initProcessor :: Platform a => a -> Processor a
initProcessor platform =
  Processor
    { flags = 0
    , registers =
        Registers
          {a = 0, b = 0, c = 0, d = 0, e = 0, h = 0, l = 0, pc = 0, sp = 0}
    , interruptsEnabled = True
    , halted = False
    , platform = platform
    }
