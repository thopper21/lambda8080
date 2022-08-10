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
  | Help
  | Error ErrorKind

parse :: String -> Action
parse line =
  case line of
    "q" -> Quit
    "h" -> Help
    _   -> Error (UnknownCommand line)

help :: IO ()
help = do
  putStrLn "Commands:"
  putStrLn "\th\tDisplay this help message"
  putStrLn "\tq\tQuit the debugger"

error :: ErrorKind -> IO ()
error (UnknownCommand command) = do
  putStrLn $ "Unknown command: '" ++ command ++ "'"
  help

evalLoop :: IO () -> IO ()
evalLoop action = do
  action
  loop

eval :: Action -> IO ()
eval Quit              = return ()
eval (Error errorKind) = evalLoop $ Repl.error errorKind
eval Help              = evalLoop help

prompt :: IO ()
prompt = do
  putStr "lambda8080> "
  hFlush stdout

loop :: IO ()
loop = do
  prompt
  action <- parse <$> getLine
  eval action
