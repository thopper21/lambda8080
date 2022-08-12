module Invaders
  ( Invaders
  , initInvaders
  , connections
  ) where

import           Control.Monad.State
import           Data.Bits
import           Data.IntMap         as IntMap
import           Data.Maybe
import           Data.Word
import           Processor

data ShiftRegister = ShiftRegister
  { value  :: Word16
  , offset :: Word8
  }

data Invaders = Invaders
  { memory        :: IntMap Word8
  , shiftRegister :: ShiftRegister
  }

type InvadersState = StateT Invaders IO

insertShift :: Word8 -> ShiftRegister -> ShiftRegister
insertShift fromPort register = register {value = newValue}
  where
    newValue = shiftL (fromIntegral fromPort) 8 .|. shiftR (value register) 8

setOffset :: Word8 -> ShiftRegister -> ShiftRegister
-- Only bits 0, 1, and 2
setOffset fromPort register = register {offset = fromPort .&. 0x7}

getValue :: ShiftRegister -> Word8
getValue register =
  fromIntegral $ shiftR (value register) (8 - fromIntegral (offset register))

modifyShiftRegister :: (ShiftRegister -> ShiftRegister) -> Invaders -> Invaders
modifyShiftRegister setter prevState =
  prevState {shiftRegister = setter $ shiftRegister prevState}

readMemory :: Word16 -> InvadersState Word8
readMemory addr =
  gets $ fromMaybe 0 . IntMap.lookup (fromIntegral addr) . memory

writeMemory :: Word16 -> Word8 -> InvadersState ()
writeMemory addr value =
  modify $ \invaders ->
    invaders {memory = insert (fromIntegral addr) value (memory invaders)}

readIn :: Word8 -> InvadersState Word8
readIn 0 = return 0x01
readIn 3 = gets $ getValue . shiftRegister
readIn _ = return 0x00

writeOut :: Word8 -> Word8 -> InvadersState ()
writeOut 4 value = modify . modifyShiftRegister . insertShift $ value
writeOut 2 value = modify . modifyShiftRegister . setOffset $ value
writeOut _ _     = return ()

connections :: Connections InvadersState
connections =
  Connections
    { Processor.readMemory = Invaders.readMemory
    , Processor.writeMemory = Invaders.writeMemory
    , Processor.readIn = Invaders.readIn
    , Processor.writeOut = Invaders.writeOut
    }

initInvaders :: [Word8] -> Invaders
initInvaders instructions =
  Invaders
    { memory = fromAscList (zip [0 ..] instructions)
    , shiftRegister = ShiftRegister {value = 0, offset = 0}
    }
