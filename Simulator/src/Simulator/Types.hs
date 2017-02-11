{-# LANGUAGE TemplateHaskell #-}

module Simulator.Types where

import Control.Lens
import Data.Word
import qualified Data.Vector.Mutable as V

type RegId = Word8

data Instr = Halt
           | Nop
           | Rrmovl RegId RegId
           | Irmovl RegId Word32
           | Rmmovl RegId RegId Word32
           | Mrmovl RegId RegId Word32
           | Addl   RegId RegId
           | Subl   RegId RegId
           | Andl   RegId RegId
           | Xorl   RegId RegId
           | Jmp    Word32
           | Jle    Word32
           | Jl     Word32
           | Je     Word32
           | Jne    Word32
           | Jge    Word32
           | Jg     Word32
           | Cmovle RegId RegId
           | Cmovl  RegId RegId
           | Cmove  RegId RegId
           | Cmovne RegId RegId
           | Cmovge RegId RegId
           | Cmovg  RegId RegId
           | Call   Word32
           | Ret
           | Pushl  RegId
           | Popl   RegId
           | Excpt

data Stat = AOK | HLT | ADR | INS
  deriving (Show, Eq)

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

type RAM = V.IOVector Word8
