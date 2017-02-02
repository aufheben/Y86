import Data.Char
import Control.Applicative
import NanoParsec
import Types

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

instruction :: Parser Instr
instruction = do
  s <- word
  case s of
    "nop"    -> return Nop
    "halt"   -> return Halt
    "rrmovl" -> reg_reg Rrmovl
    "irmovl" -> do
      c <- constant
      token $ char ','
      r <- register
      return $ Irmovl r c
    "rmmovl" -> do
      r1 <- register
      token $ char ','
      d  <- number <|> return 0
      token $ char '('
      r2 <- register
      token $ char ')'
      return $ Rmmovl r1 r2 d
    "mrmovl" -> do
      d  <- number <|> return 0
      token $ char '('
      r1 <- register
      token $ char ')'
      token $ char ','
      r2 <- register
      return $ Mrmovl r1 r2 d
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
    "ret"    -> return Ret
    "pushl"  -> reg Pushl
    "popl"   -> reg Popl
    x -> error $ "Invalid instruction: " ++ x
  where
  reg instr = do
    r <- register
    return $ instr r

  reg_reg instr = do
    r1 <- register
    token $ char ','
    r2 <- register
    return $ instr r1 r2

  lbl instr = do
    dst <- word
    return $ instr dst

register :: Parser Reg
register = do
  char '%'
  s <- word
  let r = case s of
            "eax" -> Reg 0
            "ecx" -> Reg 1
            "edx" -> Reg 2
            "ebx" -> Reg 3
            "esp" -> Reg 4
            "ebp" -> Reg 5
            "esi" -> Reg 6
            "edi" -> Reg 7
            x     -> error $ "Invalid register: " ++ x
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
  xs <- many entity
  many comment
  return xs
