module Simulator.Util
  ( decodeRegs
  , readReg
  , regSetter
  , formatCpuState
  , formatInstr
  ) where

import Control.Lens
import Data.Bits
import Data.Maybe
import Data.Word
import Simulator.Types
import Text.Printf

decodeRegs :: Word8 -> (Word8, Word8)
decodeRegs w = (shiftR w 4, w .&. 0x0F)

fun r = fromJust . lookup r

readReg :: Word8 -> CpuState -> Word32
readReg r = fun r [(0, _eax), (1, _ecx), (2, _edx), (3, _ebx),
                   (4, _esp), (5, _ebp), (6, _esi), (7, _edi)]

regSetter :: Reg -> ASetter' CpuState Word32
regSetter r = fun r [(0, eax), (1, ecx), (2, edx), (3, ebx),
                     (4, esp), (5, ebp), (6, esi), (7, edi)]

showReg :: Reg -> String
showReg r = fun r [(0, "%eax"), (1, "%ecx"), (2, "%edx"), (3, "%ebx"),
                   (4, "%esp"), (5, "%ebp"), (6, "%esi"), (7, "%edi")]

showHex :: PrintfArg a => a -> String
showHex w = printf "0x%08x" w

showFlag :: Bool -> String
showFlag True = "1"
showFlag _    = "0"

formatCpuState :: CpuState -> String
formatCpuState CpuState {..} =
  unlines
    [ unwords ["PC:", f _pc, "ZF:", g _zF, "SF:", g _sF,
               "OF:", g _oF, "STAT:", show _stat],
      unwords ["%eax:", f _eax, "%ecx:", f _ecx, "%edx:", f _edx, "%ebx:", f _ebx,
               "%esi:", f _esi, "%edi:", f _edi, "%esp:", f _esp, "%ebp:", f _ebp]
    ]
  where
  f = showHex
  g = showFlag

formatInstr :: Instr -> String
formatInstr Halt             = "halt"
formatInstr Nop              = "nop"
formatInstr (Rrmovl ra rb)   = irr "rrmovl" ra rb
formatInstr (Irmovl rb v)    = unwords ["irmovl", showHex v  ++ ",", showReg rb]
formatInstr (Rmmovl ra rb d) = unwords ["rmmovl", showReg ra ++ ",", dr d  rb]
formatInstr (Mrmovl ra rb d) = unwords ["mrmovl", dr d rb ++ ",", showReg ra]
formatInstr (Addl ra rb)     = irr "addl" ra rb
formatInstr (Subl ra rb)     = irr "subl" ra rb
formatInstr (Andl ra rb)     = irr "andl" ra rb
formatInstr (Xorl ra rb)     = irr "xorl" ra rb
formatInstr (Jmp d)          = dst "jmp" d
formatInstr (Jle d)          = dst "jle" d
formatInstr (Jl d)           = dst "jl" d
formatInstr (Je d)           = dst "je" d
formatInstr (Jne d)          = dst "jne" d
formatInstr (Jge d)          = dst "jge" d
formatInstr (Jg d)           = dst "jg" d
formatInstr (Cmovle ra rb)   = irr "cmovle" ra rb
formatInstr (Cmovl ra rb)    = irr "cmovl" ra rb
formatInstr (Cmove ra rb)    = irr "cmove" ra rb
formatInstr (Cmovne ra rb)   = irr "cmovne" ra rb
formatInstr (Cmovge ra rb)   = irr "cmovge" ra rb
formatInstr (Cmovg ra rb)    = irr "cmovg" ra rb
formatInstr (Call d)         = dst "call" d
formatInstr Ret              = "ret"
formatInstr (Pushl ra)       = "pushl " ++ showReg ra
formatInstr (Popl ra)        = "popl " ++ showReg ra
formatInstr Excpt            = "Exception"

dst i d     = unwords [i, showHex d]
irr i ra rb = unwords [i, showHex ra ++ ",", showHex rb]
dr d r      = showHex d ++ "(" ++ showReg r ++ ")"
