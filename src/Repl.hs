module Repl
  ( runRepl
  ) where

import           Control.Monad
import           Control.Monad.State
import           Data.Maybe
import           System.IO

newtype ErrorKind =
  UnknownCommand String

data Action
  = Quit
  | Help
  | Error ErrorKind

data ReplState = ReplState
  {
  }

parse :: String -> Action
parse line =
  case words line of
    ["q"]    -> Quit
    ["quit"] -> Quit
    ["h"]    -> Help
    ["help"] -> Help
    _        -> Error (UnknownCommand line)

help :: IO ()
help = do
  putStrLn "Commands:"
  putStrLn "\th(elp)\t\tDisplay this help message"
  putStrLn "\tq(uit)\t\tQuit the debugger"

error :: ErrorKind -> IO ()
error (UnknownCommand command) = do
  putStrLn $ "Unknown command: '" ++ command ++ "'"
  help

evalLoop :: StateT ReplState IO () -> StateT ReplState IO ()
evalLoop action = do
  action
  loop

eval :: Action -> StateT ReplState IO ()
eval Quit              = return ()
eval (Error errorKind) = evalLoop $ liftIO $ Repl.error errorKind
eval Help              = evalLoop $ liftIO help

prompt :: IO ()
prompt = do
  putStr "lambda8080> "
  hFlush stdout

loop :: StateT ReplState IO ()
loop = do
  liftIO prompt
  action <- parse <$> liftIO getLine
  eval action

runRepl :: IO ()
runRepl = evalStateT loop (ReplState {})
