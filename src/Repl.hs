module Repl
  ( loop
  ) where

import           Control.Monad
import           Data.Maybe
import           System.IO

newtype ErrorKind =
  UnknownCommand String

data Action
  = Quit
  | Error ErrorKind

parse :: String -> Action
parse line =
  case line of
    "q" -> Quit
    _   -> Error (UnknownCommand line)

eval :: Action -> IO ()
eval Quit = return ()
eval (Error (UnknownCommand command)) = do
  putStrLn $ "Unknown command: \"" ++ command ++ "\""
  loop

prompt :: IO ()
prompt = do
  putStr "lambda8080> "
  hFlush stdout

loop :: IO ()
loop = do
  prompt
  action <- parse <$> getLine
  eval action
