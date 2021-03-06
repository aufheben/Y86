module Assembler.Parser (entities) where

import Assembler.NanoParsec
import Assembler.Types
import Data.Maybe
import Control.Applicative

comment :: Parser ()
comment = do
  s <- string "#" <|> string "//" <|> string "/*"
  case s of
    "/*" -> skip_until "*/"
    _    -> many (satisfy (/= '\n'))
  spaces
  return ()
  where
  skip_until s = do
    many (satisfy (/= head s))
    s' <- string s <|> return ""
    case s' of
      "" -> item >> skip_until s
      x  -> return x

-- .pos 0
directive :: Parser (String, Int)
directive = do
  char '.'
  s <- word
  i <- number
  return (s, i)

-- label:
label :: Parser String
label = endsWith ':'

instruction :: Parser (Instr, Int)
instruction = do
  s <- word
  case s of
    "halt"   -> return (Halt, 1)
    "nop"    -> return (Nop, 1)
    "rrmovl" -> reg_reg Rrmovl
    "irmovl" -> do
      c <- constant
      token $ char ','
      r <- register
      return (Irmovl r c, 6)
    "rmmovl" -> do
      r1 <- register
      token $ char ','
      d  <- number <|> return 0
      token $ char '('
      r2 <- register
      token $ char ')'
      return (Rmmovl r1 r2 d, 6)
    "mrmovl" -> do
      d  <- number <|> return 0
      token $ char '('
      r2 <- register
      token $ char ')'
      token $ char ','
      r1 <- register
      return (Mrmovl r1 r2 d, 6)
    "addl"   -> reg_reg Addl
    "xorl"   -> reg_reg Xorl
    "subl"   -> reg_reg Subl
    "andl"   -> reg_reg Andl
    "jmp"    -> lbl Jmp
    "jle"    -> lbl Jle
    "jl"     -> lbl Jl
    "je"     -> lbl Je
    "jne"    -> lbl Jne
    "jge"    -> lbl Jge
    "jg"     -> lbl Jg
    "cmovle" -> reg_reg Cmovle
    "cmovl"  -> reg_reg Cmovl
    "cmove"  -> reg_reg Cmove
    "cmovne" -> reg_reg Cmovne
    "cmovge" -> reg_reg Cmovge
    "cmovg"  -> reg_reg Cmovg
    "call"   -> lbl Call
    "ret"    -> return (Ret, 1)
    "pushl"  -> reg Pushl
    "popl"   -> reg Popl
    x -> error $ "Invalid instruction: " ++ x
  where
  reg instr = do
    r <- register
    return (instr r, 2)

  reg_reg instr = do
    r1 <- register
    token $ char ','
    r2 <- register
    return (instr r1 r2, 2)

  lbl instr = do
    dst <- word
    return (instr dst, 5)

register :: Parser RegId
register = do
  char '%'
  s <- word
  let r = fromJust $ lookup s [("eax", 0), ("ecx", 1), ("edx", 2), ("ebx", 3),
                               ("esp", 4), ("ebp", 5), ("esi", 6), ("edi", 7)]
  return r

-- $15, $-1, $0xABC
immediate :: Parser Int
immediate = do
  char '$'
  number

-- $15, or label
constant :: Parser Constant
constant = (Left <$> immediate) <|> (Right <$> word)

-- label or instruction or directive (label must be parsed before instruction)
entity :: Parser Entity
entity = do
  many comment
  (Label <$> label) <|> (Instr <$> instruction) <|> (Directive <$> directive)

entities :: Parser [Entity]
entities = do
  spaces -- in case the file starts with spaces
  xs <- many entity
  many comment
  return xs
