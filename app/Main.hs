module Main where

import           Control.Monad.State
import           Data.ByteString     as BS (drop, readFile, unpack)
import           Data.IntMap
import           Data.Maybe
import           Data.Semigroup      ((<>))
import           Data.Word
import           Disassembler
import           Instruction
import           Options.Applicative
import           Processor

data DisassemblerArgs = DisassemblerArgs
  { file   :: FilePath
  , offset :: Int
  , count  :: Int
  }

disassemblerArgsParser = DisassemblerArgs <$> file' <*> offset' <*> count'
  where
    file' =
      strOption
        (long "file" <> metavar "FILE" <> help "The file to disassemble")
    offset' =
      option
        auto
        (long "offset" <> short 'o' <> help "The offset into the assembly file" <>
         metavar "INT" <>
         value 0)
    count' =
      option
        auto
        (long "count" <> short 'c' <> help "The number of instructions to read" <>
         metavar "INT" <>
         value 100)

runDisassembler args = do
  assembly <- BS.readFile $ file args
  let instructions = disassemble $ BS.unpack (BS.drop (offset args) assembly)
  mapM_ print $ take (count args) instructions

stepN :: Int -> Processor SpaceInvaders -> Int -> IO (Processor SpaceInvaders)
stepN 0 processor _ = return processor
stepN n processor i = do
  processor' <- execStateT step processor
  stepN (n - 1) processor' (i + 1)

-- Simple defaults for Space Invaders to run in attract mode
-- TODO: Implement Space Invaders shift register and move to invaders folder
newtype SpaceInvaders = SpaceInvaders
  { memory :: IntMap Word8
  }

instance Platform SpaceInvaders where
  readMemory addr =
    fromMaybe 0 . Data.IntMap.lookup (fromIntegral addr) . memory
  writeMemory addr value invaders =
    invaders {memory = insert (fromIntegral addr) value (memory invaders)}
  readIn port _ =
    case port of
      0 -> return 0x01
      _ -> return 0x00
  writeOut port value _ = return ()

initInvaders :: [Word8] -> SpaceInvaders
initInvaders instructions =
  SpaceInvaders {memory = fromAscList (zip [0 ..] instructions)}

runProcessor args = do
  assembly <- BS.readFile $ file args
  let instructions = BS.unpack assembly
  let processor = initProcessor $ initInvaders instructions
  result <- stepN (count args) processor 0
  print result

main = runProcessor =<< execParser opts
  where
    opts =
      info
        (disassemblerArgsParser <**> helper)
        (fullDesc <> progDesc "Disassemble 8080 assembly for FILE")
