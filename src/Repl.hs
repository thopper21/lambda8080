module Repl
  ( runRepl
  , runReplWithFile
  ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.State
import           Data.ByteString     as BS (ByteString, drop, readFile, unpack)
import           Data.Maybe
import           Data.Word
import           GHC.IO.Exception    (IOErrorType (InvalidArgument))
import           Invaders
import           Processor
import           System.IO
import           Text.Read

data ErrorKind
  = UnknownCommand String
  | InvalidArg String
               String
  | MissingArg String

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
    ["l"] -> Error (MissingArg "FILE")
    ["load"] -> Error (MissingArg "FILE")
    ["load", file] -> Load file
    ["r"] -> Run 1000
    ["r", arg] ->
      case readMaybe arg of
        Just numLines -> Run numLines
        Nothing       -> Error (InvalidArg "non-negative integer" arg)
    _ -> Error (UnknownCommand line)

loadWithAssembly :: [Word8] -> StateT ReplState IO ()
loadWithAssembly assembly = do
  let processor = initProcessor $ initInvaders assembly
  modify $ \state -> state {game = Just processor}

tryLoad :: String -> IO (Either IOException BS.ByteString)
tryLoad = try . BS.readFile

load :: String -> StateT ReplState IO ()
load file = do
  contents <- liftIO $ tryLoad file
  case contents of
    Left error     -> liftIO $ putStrLn $ "Invalid file name: " ++ show error
    Right assembly -> loadWithAssembly $ BS.unpack assembly

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
  putStrLn "\tr(un) N[=1000]\tRun the program through N instructions"
  putStrLn "\tl(oad) FILE\tLoad an assembly file"
  putStrLn "\th(elp)\t\tDisplay this help message"
  putStrLn "\tq(uit)\t\tQuit the debugger"

error :: ErrorKind -> IO ()
error (UnknownCommand command) = do
  putStrLn $ "Unknown command: '" ++ command ++ "'"
  help
error (InvalidArg expected actual) =
  putStrLn $ "Invalid argument. Expected " ++ expected ++ ", found " ++ actual
error (MissingArg argName) = putStrLn $ "Missing argument: " ++ argName

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

runReplWithFile :: String -> IO ()
runReplWithFile file =
  evalStateT (evalLoop (load file)) (ReplState {game = Nothing})
