module Main where

import           Control.Monad.State
import           Data.ByteString     as BS (drop, readFile, unpack)
import           Data.Semigroup      ((<>))
import           Data.Word
import           Disassembler
import           Invaders
import           Options.Applicative
import           Processor
import           Repl

newtype Args = Args
  { file :: Maybe FilePath
  }

argsParser = Args <$> file'
  where
    file' =
      optional $
      strOption
        (long "file" <> short 'f' <> metavar "FILE" <>
         help "The file to disassemble")

--runDisassembler args = do
--  assembly <- BS.readFile $ file args
--  let instructions = disassemble $ BS.unpack (BS.drop (offset args) assembly)
--  mapM_ print $ take (count args) instructions
run args = maybe runRepl runReplWithFile (file args)

main = run =<< execParser opts
  where
    opts =
      info
        (argsParser <**> helper)
        (fullDesc <> progDesc "Run lambda8080 in debug mode")
