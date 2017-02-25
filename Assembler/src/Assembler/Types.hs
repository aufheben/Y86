module Assembler.Types where

import Data.Word

-- Constant is either an int or a label which resolves to an int
type Label    = String
type Constant = Either Int Label
type RegId    = Word8

data Instr = Halt
           | Nop
           | Rrmovl RegId RegId
           | Irmovl RegId Constant
           | Rmmovl RegId RegId Int
           | Mrmovl RegId RegId Int
           | Addl   RegId RegId
           | Subl   RegId RegId
           | Andl   RegId RegId
           | Xorl   RegId RegId
           | Jmp    Label
           | Jle    Label
           | Jl     Label
           | Je     Label
           | Jne    Label
           | Jge    Label
           | Jg     Label
           | Cmovle RegId RegId
           | Cmovl  RegId RegId
           | Cmove  RegId RegId
           | Cmovne RegId RegId
           | Cmovge RegId RegId
           | Cmovg  RegId RegId
           | Call   Label
           | Ret
           | Pushl  RegId
           | Popl   RegId
    deriving Show

data Entity = Instr (Instr, Int)
            | Directive (String, Int)
            | Label Label
    deriving Show
