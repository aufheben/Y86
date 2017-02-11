{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Exception
import Simulator.ISA
import Simulator.RAM
import Simulator.Types
import Simulator.Util
import qualified Data.ByteString as B
import qualified Data.Vector.Mutable as V

step :: RAM -> CpuState -> IO (CpuState, Instr)
step ram s =
  case _stat s of
    HLT -> putStrLn "CPU halted" >> return (s, Excpt)
    ADR -> putStrLn "Invalid address" >> return (s, Excpt)
    INS -> putStrLn "Invalid instruction" >> return (s, Excpt)
    AOK -> do
      [b0] <- readBytes ram (_pc s) 1
      case b0 of
        0x00 -> halt   s
        0x10 -> nop    s
        0x20 -> rrmovl ram s
        0x30 -> irmovl ram s
        0x40 -> rmmovl ram s
        0x50 -> mrmovl ram s
        0x60 -> addl   ram s
        0x61 -> subl   ram s
        0x62 -> andl   ram s
        0x63 -> xorl   ram s
        0x70 -> jmp    ram s
        0x71 -> jle    ram s
        0x72 -> jl     ram s
        0x73 -> je     ram s
        0x74 -> jne    ram s
        0x75 -> jge    ram s
        0x76 -> jg     ram s
        0x21 -> cmovle ram s
        0x22 -> cmovl  ram s
        0x23 -> cmove  ram s
        0x24 -> cmovne ram s
        0x25 -> cmovge ram s
        0x26 -> cmovg  ram s
        0x80 -> call   ram s
        0x90 -> ret    ram s
        0xA0 -> pushl  ram s
        0xB0 -> popl   ram s
        _    -> putStrLn "Invalid instruction" >> return (s { _stat = INS }, Excpt)

loadProgram :: RAM -> B.ByteString -> IO ()
loadProgram ram prog = load 0 (B.unpack prog)
  where
  load _ []     = return ()
  load i (x:xs) = V.write ram i x >> load (i + 1) xs

runProgram :: RAM -> CpuState -> IO CpuState
runProgram ram s =
  if _stat s /= AOK
    then putStrLn (formatCpuState s) >> return s
    else fst <$> step ram s >>= runProgram ram

main :: IO ()
main = do
  putStrLn "Type 'load FILENAME' to load a program, 'quit' to exit"
  ram  <- V.replicate (2 ^ (16 :: Int)) 0
  go ram initState
  where
  go ram s = do
    putStr "> "
    xs <- words <$> getLine
    case xs of
      "quit":_ -> return ()
      "load":file:_ -> do
        -- TODO: clear RAM
        r <- try $ do
          prog <- B.readFile file
          loadProgram ram prog
        case r of
          Left (SomeException _) -> putStrLn $ "Couldn't load " ++ file
          Right _ -> putStrLn "Program loaded, type 'run' to execute or Enter to step"
        go ram initState
      "run":_ -> runProgram ram s >>= go ram
      [] -> do
        (s', i) <- step ram s
        mapM_ putStrLn [formatInstr i, formatCpuState s']
        go ram s'
      -- TODO: inspect RAM
      _ -> putStrLn "Invalid command" >> go ram s
