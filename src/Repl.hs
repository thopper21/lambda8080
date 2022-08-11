module Repl
  ( runRepl
  ) where

import           Control.Monad
import           Control.Monad.State
import           Data.ByteString     as BS (drop, readFile, unpack)
import           Data.Maybe
import           GHC.IO.Exception    (IOErrorType (InvalidArgument))
import           Invaders
import           Processor
import           System.IO
import           Text.Read

data ErrorKind
  = UnknownCommand String
  | InvalidArg String
               String

data Action
  = Load String
  | Run Word
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
    [] -> Skip
    ["q"] -> Quit
    ["quit"] -> Quit
    ["h"] -> Help
    ["help"] -> Help
    ["l", file] -> Load file
    ["load", file] -> Load file
    ["r", arg] ->
      case readMaybe arg of
        Just numLines -> Run numLines
        Nothing       -> Error (InvalidArg "non-negative integer" arg)
    _ -> Error (UnknownCommand line)

load :: String -> StateT ReplState IO ()
load file = do
  assembly <- liftIO $ BS.readFile file
  let instructions = BS.unpack assembly
  let processor = initProcessor $ initInvaders instructions
  modify $ \state -> state {game = Just processor}

runGame :: Word -> Processor Invaders -> StateT ReplState IO ()
runGame 0 currentGame = modify $ \s -> s {game = Just currentGame}
runGame n currentGame = do
  nextGame <- liftIO $ execStateT Processor.step currentGame
  runGame (n - 1) nextGame

run :: Word -> StateT ReplState IO ()
run n = do
  currentGame <- gets game
  case currentGame of
    Just g  -> runGame n g
    Nothing -> liftIO $ putStrLn "No game has been loaded."

help :: IO ()
help = do
  putStrLn "Commands:"
  putStrLn "\tl(oad) FILE\t Load an assembly file"
  putStrLn "\th(elp)\t\tDisplay this help message"
  putStrLn "\tq(uit)\t\tQuit the debugger"
  putStrLn "\tr(un) N\t\tRun the program through N instructions"

error :: ErrorKind -> IO ()
error (UnknownCommand command) = do
  putStrLn $ "Unknown command: '" ++ command ++ "'"
  help
error (InvalidArg expected actual) =
  putStrLn $ "Invalid argument. Expected " ++ expected ++ ", found " ++ actual

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
eval (Run numLines)    = evalLoop $ run numLines

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
