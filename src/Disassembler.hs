module Disassembler
  ( disassemble
  ) where

import           Data.ByteString (unpack)
import           Data.Word
import           Instruction
import           Numeric         (showHex)
import           Processor
import           Text.Printf

data Operation
  = Nullary Instruction
  | Unary Instruction
          Word8
  | Binary Instruction
           Word8
           Word8

printOp :: Operation -> IO ()
printOp (Nullary instruction) = printf "%s" (shows instruction "")
printOp (Unary instruction arg) = printf "%s %02x" (shows instruction "") arg
printOp (Binary instruction high low) =
  printf "%s %02x %02x" (shows instruction "") high low

operationWidth (Nullary _)    = 1
operationWidth (Unary _ _)    = 2
operationWidth (Binary _ _ _) = 3

disassemble :: Platform a => Word8 -> Processor a -> IO ()
disassemble count processor = disassemble' count processor 0
  where
    disassemble' 0 _ _ = return ()
    disassemble' count processor offset = do
      let (addr, operation) = getOperation offset processor
      printf "%04x " addr
      printOp operation
      putStrLn ""
      disassemble' (count - 1) processor (offset + operationWidth operation)

getOperation :: Platform a => Word16 -> Processor a -> (Word16, Operation)
getOperation offset processor =
  case instruction of
    LXI _ -> binary
    MVI _ -> unary
    SHLD  -> binary
    LHLD  -> binary
    STA   -> binary
    LDA   -> binary
    JNZ   -> binary
    JMP   -> binary
    CNZ   -> binary
    ADI   -> unary
    JZ    -> binary
    CZ    -> binary
    CALL  -> binary
    ACI   -> unary
    JNC   -> binary
    OUT   -> unary
    CNC   -> binary
    SUI   -> unary
    JC    -> binary
    IN    -> unary
    CC    -> binary
    SBI   -> unary
    JPO   -> binary
    CPO   -> binary
    ANI   -> unary
    JPE   -> binary
    CPE   -> binary
    XRI   -> unary
    JP    -> binary
    CP    -> binary
    ORI   -> unary
    JM    -> binary
    CM    -> binary
    CPI   -> unary
    op    -> nullary
  where
    (addr, opCode) = getPC offset processor
    instruction = toInstruction opCode
    nullary = (addr, Nullary instruction)
    unary = (addr, Unary instruction arg)
      where
        (_, arg) = getPC (offset + 1) processor
    binary = (addr, Binary instruction high low)
      where
        (_, low) = getPC (offset + 1) processor
        (_, high) = getPC (offset + 2) processor
