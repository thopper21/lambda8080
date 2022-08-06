module Disassembler
  ( disassemble
  ) where

import           Data.ByteString (unpack)
import           Data.Word
import           Instruction
import           Numeric         (showHex)

data Function
  = Nullary Instruction
  | Unary Instruction
          Word8
  | Binary Instruction
           Word8
           Word8

instance Show Function where
  show (Nullary instruction) = show instruction
  show (Unary instruction byte) = show instruction ++ " " ++ showHex byte ""
  show (Binary instruction low high) =
    show instruction ++ " " ++ showHex high " " ++ showHex low ""

disassemble [] = []
disassemble (byte:bytes) =
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
    instruction = toInstruction byte
    nullary = Nullary instruction : disassemble bytes
    unary = Unary instruction (head bytes) : disassemble (tail bytes)
    binary =
      Binary instruction (head bytes) (head . tail $ bytes) :
      disassemble (tail . tail $ bytes)
