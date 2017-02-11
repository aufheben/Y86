{-# LANGUAGE TemplateHaskell #-}

module Simulator.Types where

import Control.Lens
import Data.Word

type Reg   = Word8

data Instr = Halt
           | Nop
           | Rrmovl Reg Reg
           | Irmovl Reg Word32
           | Rmmovl Reg Reg Word32
           | Mrmovl Reg Reg Word32
           | Addl   Reg Reg
           | Subl   Reg Reg
           | Andl   Reg Reg
           | Xorl   Reg Reg
           | Jmp    Word32
           | Jle    Word32
           | Jl     Word32
           | Je     Word32
           | Jne    Word32
           | Jge    Word32
           | Jg     Word32
           | Cmovle Reg Reg
           | Cmovl  Reg Reg
           | Cmove  Reg Reg
           | Cmovne Reg Reg
           | Cmovge Reg Reg
           | Cmovg  Reg Reg
           | Call   Word32
           | Ret
           | Pushl  Reg
           | Popl   Reg
           | Invalid

data Stat = AOK | HLT | ADR | INS
  deriving Show

data CpuState = CpuState
  { _eax  :: Word32
  , _ecx  :: Word32
  , _edx  :: Word32
  , _ebx  :: Word32
  , _esi  :: Word32
  , _edi  :: Word32
  , _esp  :: Word32
  , _ebp  :: Word32
  , _pc   :: Word32
  , _zF   :: Bool
  , _sF   :: Bool
  , _oF   :: Bool
  , _stat :: Stat
  }

makeLenses ''CpuState

initState :: CpuState
initState = CpuState 0 0 0 0 0 0 0 0 0 False False False AOK
