# Y86
A Y86 CPU Implemented with [CλaSH](http://www.clash-lang.org/), together with the
assembler, simulator, C compiler, etc. in Haskell.

We try to write everything from scratch, as the initial motivation to start this
project is to reinvent the wheels and perhaps have some fun along the way.

At the moment each subproject has to be built individually. For example:

```bash
# install `yas` to ~/.local/bin
cd Assembler
stack setup
stack install

# generate object file (.ybo)
yas asm/asum.ys
```

## Assembler

In Haskell terms, an assembler can be thought as a function of the type
`String -> ByteString`, where `String` represents the input Y86 assembly code,
and `ByteString` represents the output object code.

In Y86 assembly, the code and static data can be interleaved, with directives such as
`.pos` and `.align` affecting their placement. Labels are used to mark addresses.
Therefore the program can be represented as a list of entities, which can be
instructions, directives, or labels:

```haskell
type Label   = String

data Entity  = Instr (Instr, Int)      -- an instruction and the number of bytes to encode it
             | Directive (String, Int) -- a directive in Y86 carries a constant
             | Label Label             -- a label marks an address

type Program = [Entity]
```

The `Instr` type is a direct translation from the Y86 instructions:

```haskell
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
or define it directly with a parser combinator library, such as [Parsec](https://hackage.haskell.org/package/parsec).
However, Parsec might be a bit overkill for parsing the Y86 assembly, we can
actually keep things easy by defining our own parser combinators.

The [NanoParsec](http://dev.stephendiehl.com/fun/002_parsers.html)
by [Stephen Diehl](http://www.stephendiehl.com/) is very simple and shows how
a parser combinator library can be constructed. Let's steal it and make some
small changes. With NanoParsec, a parser for an entity is simply:

```haskell
entity :: Parser Entity
entity = do
  many comment -- skip comments
  (Label <$> label) <|> (Instr <$> instruction) <|> (Directive <$> directive)
```

Neat.

### Assembling

During assembling a serialization library such as [cereal](http://hackage.haskell.org/package/cereal)
would be very helpful, however we need to track the _current address_ as a state, therefore we
could use a monad transformer to build our custom `Put` monad:

```haskell
type Put = WriterT Builder (State Int) ()
```

where `Builder` comes from `Data.ByteString.Builder` for building the `ByteString`, and
`Int` represents the current address.

The first step in assembling is to build the _label to address_ mapping of type `Map Label Int`
by traversing the `[Entity]` (done with `genLabelMap`). Note that during the process the
`.align N` directive will add some padding to the code so that the following address will
be divisible by N, while `.byte`, `.word` and `.long` will add 1, 2 and 4 bytes respectively.

`putEntity` creates a `Put` for an instruction or directive (labels won't _put_ anything).

```
putEntity :: Map Label Int -> Entity -> Put
```

Now we just need to map this function into our list of entities to obtain a list of `Put` monads,
and use `sequence_` to combine them into one big `Put`. Run it, we'll have the desired
`Builder`. Use `toLazyByteString` to execute it, and we are done.

## Simulator
