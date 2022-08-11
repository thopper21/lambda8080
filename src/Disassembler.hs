module Disassembler
  ( disassemble
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.ByteString           (unpack)
import           Data.Word
import           Instruction
import           Numeric                   (showHex)
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
operationWidth Binary{} = 3

disassemble :: Word8 -> Processor (State a) -> a -> IO ()
disassemble count processor innerState = disassemble' count processor 0
  where
    disassemble' 0 _ _ = return ()
    disassemble' count processor offset = do
      let (addr, operation) = evalState (getOperation offset processor) innerState
      liftIO $ printf "%04x " addr
      liftIO $ printOp operation
      liftIO $ putStrLn ""
      disassemble' (count - 1) processor (offset + operationWidth operation)

getOperation :: Word16 -> Processor (State a) -> State a (Word16, Operation)
getOperation offset processor = do
  instruction <- getInstruction
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
    getInstruction = toInstruction <$> opCode
    nullary = do
      instruction <- getInstruction
      return (addr, Nullary instruction)
    unary = do
      instruction <- getInstruction
      arg <- getArg
      return (addr, Unary instruction arg)
      where
        (_, getArg) = getPC (offset + 1) processor
    binary = do
      instruction <- getInstruction
      low <- getLow
      high <- getHigh
      return (addr, Binary instruction high low)
      where
        (_, getLow) = getPC (offset + 1) processor
        (_, getHigh) = getPC (offset + 2) processor
