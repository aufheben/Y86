module Types where

-- Constant is either an int or a label which resolves to an int
type Constant = Either Int String

newtype Reg = Reg Int
        deriving Show

data Instr = Nop
           | Halt
           | Rrmovl Reg Reg
           | Irmovl Reg Constant
           | Rmmovl Reg Reg Int
           | Mrmovl Reg Reg Int
           | Addl   Reg Reg
           | Subl   Reg Reg
           | Andl   Reg Reg
           | Xorl   Reg Reg
           | Jmp    Constant
           | Jle    Constant
           | Jl     Constant
           | Je     Constant
           | Jne    Constant
           | Jge    Constant
           | Jg     Constant
           | Cmovle Reg Reg
           | Cmovl  Reg Reg
           | Cmove  Reg Reg
           | Cmovne Reg Reg
           | Cmovge Reg Reg
           | Cmovg  Reg Reg
           | Call   Constant
           | Ret
           | Pushl  Reg
           | Popl   Reg
    deriving Show

data Entity = Instr Instr
            | Directive (String, Int)
            | Label String
    deriving Show
