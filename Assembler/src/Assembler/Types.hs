module Assembler.Types where

import Data.Word

-- Constant is either an int or a label which resolves to an int
type Label    = String
type Constant = Either Int Label
type Reg      = Word8

data Instr = Halt
           | Nop
           | Rrmovl Reg Reg
           | Irmovl Reg Constant
           | Rmmovl Reg Reg Int
           | Mrmovl Reg Reg Int
           | Addl   Reg Reg
           | Subl   Reg Reg
           | Andl   Reg Reg
           | Xorl   Reg Reg
           | Jmp    Label
           | Jle    Label
           | Jl     Label
           | Je     Label
           | Jne    Label
           | Jge    Label
           | Jg     Label
           | Cmovle Reg Reg
           | Cmovl  Reg Reg
           | Cmove  Reg Reg
           | Cmovne Reg Reg
           | Cmovge Reg Reg
           | Cmovg  Reg Reg
           | Call   Label
           | Ret
           | Pushl  Reg
           | Popl   Reg
    deriving Show

data Entity = Instr (Instr, Int)
            | Directive (String, Int)
            | Label Label
    deriving Show
