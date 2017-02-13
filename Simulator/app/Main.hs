{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Exception.Lifted
import Control.Monad.IO.Class
import Control.Monad.Reader
import Prelude hiding (putStrLn)
import Simulator.ISA
import Simulator.RAM
import Simulator.Types
import Simulator.Util
import System.Environment
import System.IO (hFlush, stdout)
import qualified Data.ByteString as B
import qualified Data.Vector.Mutable as V

step :: CpuState -> SimIO (CpuState, Instr)
step s =
  case _stat s of
    HLT -> putStrLn "CPU halted" >> return (s, Excpt)
    ADR -> putStrLn "Invalid address" >> return (s, Excpt)
    INS -> putStrLn "Invalid instruction" >> return (s, Excpt)
    AOK -> do
      [b0] <- readBytes (_pc s) 1
      case b0 of
        0x00 -> halt   s
        0x10 -> nop    s
        0x20 -> rrmovl s
        0x30 -> irmovl s
        0x40 -> rmmovl s
        0x50 -> mrmovl s
        0x60 -> addl   s
        0x61 -> subl   s
        0x62 -> andl   s
        0x63 -> xorl   s
        0x70 -> jmp    s
        0x71 -> jle    s
        0x72 -> jl     s
        0x73 -> je     s
        0x74 -> jne    s
        0x75 -> jge    s
        0x76 -> jg     s
        0x21 -> cmovle s
        0x22 -> cmovl  s
        0x23 -> cmove  s
        0x24 -> cmovne s
        0x25 -> cmovge s
        0x26 -> cmovg  s
        0x80 -> call   s
        0x90 -> ret    s
        0xA0 -> pushl  s
        0xB0 -> popl   s
        _    -> return (s { _stat = INS }, Excpt)

loadProgram :: B.ByteString -> SimIO ()
loadProgram prog = ask >>= load 0 (B.unpack prog)
  where
  load _ []     _   = return ()
  load i (x:xs) ram = liftIO (V.write ram i x) >> load (i + 1) xs ram

runProgram :: CpuState -> SimIO CpuState
runProgram s =
  if _stat s /= AOK
    then putStrLn (formatCpuState s) >> return s
    else fst <$> step s >>= runProgram

main :: IO ()
main = do
  ram  <- V.replicate (2 ^ (16 :: Int)) 0
  args <- getArgs
  case args of
    []  -> do
      putStrLn "Type 'load FILENAME' to load a program, 'quit' to exit"
      runReaderT (go initState) ram
    f:_ -> do
      runReaderT (load f) ram
  where
  go :: CpuState -> SimIO ()
  go s = do
    -- why is hFlush needed (but not when calling main from ghci)
    liftIO $ putStr "> " >> hFlush stdout
    xs <- words <$> liftIO getLine
    case xs of
      "quit":_      -> return ()
      "load":file:_ -> load file
      []            -> stepi s
      "run":_       -> runProgram s >>= go
      "reset":_     -> clearRAM >> go initState
      cmd:addr:_ | take 2 cmd == "x/" -> xram s cmd addr
      _ -> putStrLn "Invalid command" >> go s

  load file = do
    r <- try $ do
      prog <- liftIO $ B.readFile file
      loadProgram prog
    case r of
      Left (SomeException _) -> putStrLn $ "Couldn't load " ++ file
      Right _                -> help
    go initState

  -- TODO: break, frame, and more commands from GDB
  help = putStrLn . unlines $
    [ "Program loaded, now you can type:"
    , "- Enter to step an instruction"
    , "- 'run' to start or continue execution"
    , "- 'reset' to reset CPU states and memory"
    , "- 'x/N ADDR' to examine N bytes of memory at address ADDR"
    ]

  stepi s = do
    (s', i) <- step s
    mapM_ putStrLn [formatInstr i, formatCpuState s']
    go s'

  xram s cmd addr =
    handle (\(SomeException _) -> putStrLn "failed to parse command" >> go s) $ do
      let n = read (drop 2 cmd) :: Int
          a = read addr :: Int
      printRAM a n
      go s
