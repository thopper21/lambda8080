module Main where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data DisassemblerArgs = DisassemblerArgs
  { file   :: FilePath
  , offset :: Int
  , length :: Int
  }

disassemblerArgsParser :: Parser DisassemblerArgs
disassemblerArgsParser =
  DisassemblerArgs <$>
  strOption (long "file" <> metavar "FILE" <> help "The file to disassemble") <*>
  option
    auto
    (long "offset" <> short 'o' <> help "The offset into the assembly file" <>
     metavar "INT" <>
     value 1) <*>
  option
    auto
    (long "length" <> short 'l' <> help "The number of instructions to read" <>
     metavar "INT" <>
     value 100)

disassemble :: DisassemblerArgs -> IO ()
disassemble args = putStrLn $ file args

main :: IO ()
main = disassemble =<< execParser opts
  where
    opts =
      info
        (disassemblerArgsParser <**> helper)
        (fullDesc <> progDesc "Disassemble 8080 assembly for FILE")
