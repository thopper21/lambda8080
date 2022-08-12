module Invaders
  ( Invaders
  , initInvaders
  , connections
  ) where

import           Control.Monad.State
import           Data.IntMap                as IntMap
import           Data.Maybe
import           Data.Word
import           Processor

newtype Invaders = Invaders
  { memory :: IntMap Word8
  }

type InvadersState = StateT Invaders IO

readMemory :: Word16 -> InvadersState Word8
readMemory addr =
  gets $ fromMaybe 0 . IntMap.lookup (fromIntegral addr) . memory

writeMemory :: Word16 -> Word8 -> InvadersState ()
writeMemory addr value =
  modify $ \invaders ->
    invaders {memory = insert (fromIntegral addr) value (memory invaders)}

readIn :: Word8 -> InvadersState Word8
readIn port =
  case port of
    0 -> return 0x01
    _ -> return 0x00

writeOut :: Word8 -> Word8 -> InvadersState ()
writeOut port value = return ()

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
  Invaders {memory = fromAscList (zip [0 ..] instructions)}
