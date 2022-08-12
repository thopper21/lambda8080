module Repl
  ( runRepl
  , runReplWithFile
  ) where

import           Control.Exception
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.ByteString        as BS (ByteString, drop, readFile,
                                               unpack)
import           Data.Word
import           Disassembler           (disassemble)
import           GHC.IO.Exception       (IOErrorType (InvalidArgument))
import           Instruction
import           Invaders
import           Processor
import           System.IO
import           Text.Printf
import           Text.Read

data ErrorKind
  = UnknownCommand String
  | InvalidArg String
               String
  | MissingArg String

data Action
  = Load String
  | Run Word
  | Print Word8
  | Help
  | Quit
  | Skip
  | Error ErrorKind

data GameState = GameState
  { processor :: Processor (StateT Invaders IO)
  , game      :: Invaders
  }

type ReplState = StateT (Maybe GameState) IO

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
    ["run"] -> Run 1000
    ["run", arg] ->
      case readMaybe arg of
        Just numLines -> Run numLines
        Nothing       -> Error (InvalidArg "non-negative integer" arg)
    ["p"] -> Print 10
    ["print"] -> Print 10
    ["p", arg] ->
      case readMaybe arg of
        Just numLines -> Print numLines
        Nothing       -> Error (InvalidArg "non-negative integer" arg)
    ["print", arg] ->
      case readMaybe arg of
        Just numLines -> Print numLines
        Nothing       -> Error (InvalidArg "non-negative integer" arg)
    _ -> Error (UnknownCommand line)

loadWithAssembly :: [Word8] -> ReplState ()
loadWithAssembly assembly = do
  let processor = initProcessor Invaders.connections
  let game = initInvaders assembly
  put $ Just GameState {processor = processor, game = game}

tryLoad :: String -> IO (Either IOException BS.ByteString)
tryLoad = try . BS.readFile

load :: String -> ReplState ()
load file = do
  contents <- liftIO $ tryLoad file
  case contents of
    Left error     -> liftIO $ putStrLn $ "Invalid file name: " ++ show error
    Right assembly -> loadWithAssembly $ BS.unpack assembly

withProgram :: (GameState -> ReplState ()) -> ReplState ()
withProgram cont = do
  current <- Control.Monad.State.get
  case current of
    Nothing   -> liftIO $ putStrLn "No program has been loaded"
    Just game -> cont game

runProgram :: Word -> GameState -> ReplState ()
runProgram 0 currentState = put $ Just currentState
runProgram n currentState = do
  let currentProcessor = processor currentState
  let currentGame = game currentState
  (nextProcessor, nextGame) <-
    liftIO $ runStateT (execStateT Processor.step currentProcessor) currentGame
  runProgram (n - 1) $ GameState {processor = nextProcessor, game = nextGame}

run :: Word -> ReplState ()
run = withProgram . runProgram

printProgram :: Word8 -> GameState -> IO ()
printProgram numLines program = do
  let currentProcessor = processor program
  let currentGame = game program
  let af = getRegister PSW currentProcessor
  let bc = getRegister BC currentProcessor
  let de = getRegister DE currentProcessor
  let hl = getRegister HL currentProcessor
  let pc = getRegister PC currentProcessor
  let sp = getRegister SP currentProcessor
  printf "  af    bc    de    hl    pc    sp\n"
  printf " %04x  %04x  %04x  %04x  %04x  %04x\n" af bc de hl pc sp
  disassemble numLines currentProcessor currentGame

print :: Word8 -> ReplState ()
print numLines = withProgram $ liftIO . printProgram numLines

help :: IO ()
help = do
  putStrLn "Commands:"
  putStrLn "  r(un)   N[=1000] Run the program through N instructions"
  putStrLn "  l(oad)  FILE     Load an assembly file"
  putStrLn "  p(rint) N[=10]   Print the current state of the program"
  putStrLn "  h(elp)           Display this help message"
  putStrLn "  q(uit)           Quit the debugger"

error :: ErrorKind -> IO ()
error (UnknownCommand command) = do
  putStrLn $ "Unknown command: '" ++ command ++ "'"
  help
error (InvalidArg expected actual) =
  putStrLn $ "Invalid argument. Expected " ++ expected ++ ", found " ++ actual
error (MissingArg argName) = putStrLn $ "Missing argument: " ++ argName

evalLoop :: ReplState () -> ReplState ()
evalLoop action = do
  action
  loop

eval :: Action -> ReplState ()
eval Skip              = evalLoop $ return ()
eval Quit              = return ()
eval (Error errorKind) = evalLoop $ liftIO $ Repl.error errorKind
eval Help              = evalLoop $ liftIO help
eval (Load file)       = evalLoop $ load file
eval (Run numLines)    = evalLoop $ run numLines
eval (Print numLines)  = evalLoop $ Repl.print numLines

prompt :: IO ()
prompt = do
  putStr "lambda8080> "
  hFlush stdout

loop :: ReplState ()
loop = do
  liftIO prompt
  action <- parse <$> liftIO getLine
  eval action

runRepl :: IO ()
runRepl = evalStateT loop Nothing

runReplWithFile :: String -> IO ()
runReplWithFile file = evalStateT (evalLoop (load file)) Nothing
