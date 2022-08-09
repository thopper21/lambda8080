module Main where

import           Data.ByteString     as BS (drop, readFile, unpack)
import           Data.Semigroup      ((<>))
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

stepN :: Int -> Processor -> Int -> IO Processor
stepN 0 processor _ = return processor
stepN n processor i = do
  let (opCode, processor') = step processor
  stepN (n - 1) processor' (i + 1)

runProcessor args = do
  assembly <- BS.readFile $ file args
  let instructions = BS.unpack assembly
  let processor = rom instructions 0xffff
  result <- stepN (count args) processor 0
  print result

main = runProcessor =<< execParser opts
  where
    opts =
      info
        (disassemblerArgsParser <**> helper)
        (fullDesc <> progDesc "Disassemble 8080 assembly for FILE")
