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
import           Disassembler        (disassemble)
import           GHC.IO.Exception    (IOErrorType (InvalidArgument))
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

newtype ReplState = ReplState
  { program :: Maybe (Processor Invaders)
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

loadWithAssembly :: [Word8] -> StateT ReplState IO ()
loadWithAssembly assembly = do
  let processor = initProcessor $ initInvaders assembly
  modify $ \state -> state {program = Just processor}

tryLoad :: String -> IO (Either IOException BS.ByteString)
tryLoad = try . BS.readFile

load :: String -> StateT ReplState IO ()
load file = do
  contents <- liftIO $ tryLoad file
  case contents of
    Left error     -> liftIO $ putStrLn $ "Invalid file name: " ++ show error
    Right assembly -> loadWithAssembly $ BS.unpack assembly

withProgram ::
     (Processor Invaders -> StateT ReplState IO ()) -> StateT ReplState IO ()
withProgram cont = do
  currentProgram <- gets program
  case currentProgram of
    Just program -> cont program
    Nothing      -> liftIO $ putStrLn "No program has been loaded"

runProgram :: Word -> Processor Invaders -> StateT ReplState IO ()
runProgram 0 currentProgram = modify $ \s -> s {program = Just currentProgram}
runProgram n currentProgram = do
  nextProgram <- liftIO $ execStateT Processor.step currentProgram
  runProgram (n - 1) nextProgram

run :: Word -> StateT ReplState IO ()
run = withProgram . runProgram

printProgram :: Word8 -> Processor Invaders -> IO ()
printProgram numLines program = do
  let af = getRegister PSW program
  let bc = getRegister BC program
  let de = getRegister DE program
  let hl = getRegister HL program
  let pc = getRegister PC program
  let sp = getRegister SP program
  printf "  af    bc    de    hl    pc    sp\n"
  printf " %04x  %04x  %04x  %04x  %04x  %04x\n" af bc de hl pc sp
  disassemble numLines program

print :: Word8 -> StateT ReplState IO ()
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
eval (Print numLines)  = evalLoop $ Repl.print numLines

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
runRepl = evalStateT loop (ReplState {program = Nothing})

runReplWithFile :: String -> IO ()
runReplWithFile file =
  evalStateT (evalLoop (load file)) (ReplState {program = Nothing})
