module Repl
  ( runRepl
  ) where

import           Control.Monad
import           Control.Monad.State
import           Data.ByteString     as BS (drop, readFile, unpack)
import           Data.Maybe
import           Invaders
import           Processor
import           System.IO

newtype ErrorKind =
  UnknownCommand String

data Action
  = Load String
  | Help
  | Quit
  | Skip
  | Error ErrorKind

newtype ReplState = ReplState
  { game :: Maybe (Processor Invaders)
  }

parse :: String -> Action
parse line =
  case words line of
    []             -> Skip
    ["q"]          -> Quit
    ["quit"]       -> Quit
    ["h"]          -> Help
    ["help"]       -> Help
    ["l", file]    -> Load file
    ["load", file] -> Load file
    _              -> Error (UnknownCommand line)

load :: String -> StateT ReplState IO ()
load file = do
  assembly <- liftIO $ BS.readFile file
  let instructions = BS.unpack assembly
  let processor = initProcessor $ initInvaders instructions
  modify $ \state -> state {game = Just processor}

help :: IO ()
help = do
  putStrLn "Commands:"
  putStrLn "\tl(oad) FILE\t Load an assembly file"
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
eval Skip              = evalLoop $ return ()
eval Quit              = return ()
eval (Error errorKind) = evalLoop $ liftIO $ Repl.error errorKind
eval Help              = evalLoop $ liftIO help
eval (Load file)       = evalLoop $ load file

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
runRepl = evalStateT loop (ReplState {game = Nothing})
