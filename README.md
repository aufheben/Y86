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
cd asm
yas asum.ys

# install `yis` to ~/.local/bin
cd ../../Simulator
stack setup
stack install

# run program in the simulator
yis asm/asum.ybo
> run
PC: 0x00000012 ZF: 1 SF: 0 OF: 0 STAT: HLT
%eax: 0x0000abcd %ecx: 0x00000024 %edx: 0x00000000 %ebx: 0xffffffff
%esi: 0x0000a000 %edi: 0x00000000 %esp: 0x00000100 %ebp: 0x00000100
```

Implementation highlights are documented below.

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

type RegId    = Word8

data Instr    = Nop
              | Halt
              | Rrmovl RegId RegId
              | Irmovl RegId Constant
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
or define it directly with a parser combinator library such as [Parsec](https://hackage.haskell.org/package/parsec).
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

```haskell
putEntity :: Map Label Int -> Entity -> Put
```

Now we just need to map this function into our list of entities to obtain a list of `Put` monads,
and use `sequence_` to combine them into one big `Put`. Run it, we'll have the desired
`Builder`. Use `toLazyByteString` to execute it, and we are done.

## Simulator

An _instruction level simulator_ simply implements the operational semantics of the Y86
instruction set. Five groups of programmer-visible state need to be modeled: the
program registers, the condition code, the program counter, the status code, and the memory.
We think of the memory as external to the CPU and the previous 4 groups as internal states.

The memory is modeled as a [mutable vector](https://hackage.haskell.org/package/vector/docs/Data-Vector-Mutable.html)
of bytes (Word8). The internal states are modeled by a State monad. We could have modeled the
internal states as a mutable vector as well, but the reason for this division is that it
gives the best overall structure and makes it easy to transform the simulator into
synthesizable CλaSH code.

```haskell
import qualified Data.Vector.Mutable as V

type RAM   = V.IOVector Word8

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
```

The RAM is hidden in a Reader monad so that we don't have to pass it around everywhere:

```haskell
type SimIO = ReaderT RAM IO
```

[RAM.hs](https://github.com/aufheben/Y86/blob/master/Simulator/src/Simulator/RAM.hs)
contains the RAM-related utilty functions.

The workflow of simulator is straightforward: the program (.ybo file) is first loaded
into RAM, then the user could either hit Enter to step over the instructions, or type `run`
to execute the program and observe the final CPU states. Use `x/N ADDR` (borrowed from GDB)
to examine N bytes of memory at addresses ADDR. `reset` will reset the CPU states as well as
the memory.

To execute an instruction, the `step` function takes an old CpuState and returns a new
CpuState along with the instruction just executed. The first byte pointed by PC is read
from RAM and checked to determine which instruction to execute:

```haskell
step :: CpuState -> SimIO (CpuState, Instr)
step s =
  case _stat s of
    HLT -> putStrLn "CPU halted" >> return (s, Excpt)
    ADR -> putStrLn "Invalid address" >> return (s, Excpt)
    INS -> putStrLn "Invalid instruction" >> return (s, Excpt)
    AOK -> do
      [b0] <- readBytes (_pc s) 1
      case b0 of
        0x00 -> halt   s
        0x10 -> nop    s
        ...
```

The `Instr` here is borrowed from the assembler but defined slightly differently, in that
all the labels are replaced with the actual Word32 addresses. It also includes an `Excpt`
case to represent an exception.

To run the program, `step` is called repeatedly until the status code is not AOK:

```haskell
runProgram :: CpuState -> SimIO CpuState
runProgram s =
  if _stat s /= AOK
    then putStrLn (formatCpuState s) >> return s
    else fst <$> step s >>= runProgram

```

The meat of the simulator, i.e. the actual ISA implementation, can be found in [ISA.hs](https://github.com/aufheben/Y86/blob/master/Simulator/src/Simulator/ISA.hs).
The combination of the `RecordWildCards` GHC extension and the `MonadState` combinators
from [lens](https://hackage.haskell.org/package/lens/docs/Control-Lens-Setter.html#g:5)
makes it quite pleasant to write code in an "imperative style":

```haskell
exec s i a = return (flip execState s a, i)

mrmovl s@(CpuState {..}) = do
  (ra, rb, d) <- readRRL _pc
  let addr = readReg rb s + d
  v <- readWord32 addr
  exec s (Mrmovl ra rb d) $ do
    regSetter ra .= v
    pc += 6
```
