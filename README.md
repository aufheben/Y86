# Y86
A Y86 CPU Implemented with [CλaSH](http://www.clash-lang.org/), together with the
assembler, simulator, C compiler, etc. in Haskell.

We try to write everything from scratch, as the initial motivation to start this
project is to reinvent the wheels and perhaps have some fun along the way.

## Assembler

In Haskell terms, an assembler can be thought as a function of the type
`String -> ByteString`, where `String` represents the input Y86 assembly code,
and `ByteString` represents the output object code.

In Y86 assembly, the code and static data can be interleaved, with directives such as
`.pos` and `.align` affecting their placement. Labels are used to mark addresses.
Therefore the program can be represented as a list of entities which can be
instructions, directives, or labels:

```
type Label   = String

data Entity  = Instr (Instr, Int)      -- an instruction and the number of bytes to encode it
             | Directive (String, Int) -- a directive in Y86 carries a constant
             | Label Label             -- a label marks an address

type Program = [Entity]
```

The `Instr` type is a direct translation from the Y86 instructions:

```
-- Constant is either an int or a label which resolves to an int
type Constant = Either Int Label

type Reg      = Word8

data Instr    = Nop
              | Halt
              | Rrmovl Reg Reg
              | Irmovl Reg Constant
              | ...
              | Jmp    Label
              | ...
```

The assembler can be defined in 3 steps:

1. Parse the assembly code into a list of entities: `parse :: String -> [Entity]`
2. Assemble the list of entities into the binary form
   `assemble :: [Entity] -> ByteString`
3. The assembler is simply the composition of these two functions:
   `assembler = assemble . parse`

### Parsing

Now, to write `parse`, we could use a parser generator such as [Happy](https://www.haskell.org/happy/doc/html/index.html),
or define it directly with a parser combinator library, such as Parsec. However,
since the Y86 assembly is very simple, why not define our own parser combinators,
keeping the spirit of "reinventing the wheel".

The [NanoParsec](http://dev.stephendiehl.com/fun/002_parsers.html)
by [Stephen Diehl](http://www.stephendiehl.com/) is very simple and shows how
a parser combinator library can be constructed. Let's steal it and make some
small changes. With NanoParsec, a parser for an entity is simply:

```
entity :: Parser Entity
entity = do
  many comment -- skip comments
  (Label <$> label) <|> (Instr <$> instruction) <|> (Directive <$> directive)
```

Neat.

### Assembling
