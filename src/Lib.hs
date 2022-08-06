module Lib
    ( someFunc
    ) where

import Instruction

someFunc :: IO ()
someFunc = print (toInstruction 0x01)
