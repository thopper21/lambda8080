module Invaders
  ( Invaders
  , initInvaders
  ) where

import           Data.IntMap as IntMap
import           Data.Maybe
import           Data.Word
import           Processor

newtype Invaders = Invaders
  { memory :: IntMap Word8
  }

instance Platform Invaders where
  readMemory addr = fromMaybe 0 . IntMap.lookup (fromIntegral addr) . memory
  writeMemory addr value invaders =
    invaders {memory = insert (fromIntegral addr) value (memory invaders)}
  -- Simple defaults for Space Invaders to run in attract mode
  -- TODO: Implement Space Invaders shift register
  readIn port _ =
    case port of
      0 -> return 0x01
      _ -> return 0x00
  writeOut port value _ = return ()

initInvaders :: [Word8] -> Invaders
initInvaders instructions =
  Invaders {memory = fromAscList (zip [0 ..] instructions)}
