import Control.Monad.State
import Control.Monad.Writer
import Data.Bits
import Data.ByteString.Builder
import Data.Map hiding (map)
import Data.Maybe
import Data.Word
import NanoParsec
import Parser
import Prelude hiding (lookup)
import Types
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as B

type Put = WriterT Builder (State Int) ()

putWord8 :: Word8 -> Put
putWord8 = tell . B.word8

putWord16le :: Word16 -> Put
putWord16le = tell . B.word16LE

putWord32le :: Word32 -> Put
putWord32le = tell . B.word32LE

genLabelMap :: [Entity] -> Map Label Int
genLabelMap xs = go xs empty 0
  where
    go    []     mp _    = mp
    go li@(x:xs) mp addr =
      case x of
        Instr (i, n)     -> go xs mp (addr + n)
        Directive (s, n) -> case s of
                              "pos"   -> go xs mp n
                              "align" -> if addr `mod` n == 0
                                           then go xs mp addr
                                           else go li mp (addr + 1)
                              "byte"  -> go xs mp (addr + 1)
                              "word"  -> go xs mp (addr + 2)
                              "long"  -> go xs mp (addr + 4)
                              _       -> error "Invalid directive"
        Label   s        ->  go xs (insert s addr mp) addr

putInstr :: Map Label Int -> Instr -> Put
putInstr mp instr =
  case instr of
    Halt           -> putWord8 0x00
    Nop            -> putWord8 0x10
    Rrmovl r1 r2   -> put_2bytes 0x20 r1 r2
    Irmovl r2 v    -> do
      put_2bytes 0x30 0xF r2
      case v of
        Left n      -> put_word32 n
        Right label -> put_label label
    Rmmovl r1 r2 d -> put_2bytes 0x40 r1 r2 >> put_word32 d
    Mrmovl r1 r2 d -> put_2bytes 0x50 r1 r2 >> put_word32 d
    Addl   r1 r2   -> put_2bytes 0x60 r1 r2
    Subl   r1 r2   -> put_2bytes 0x61 r1 r2
    Andl   r1 r2   -> put_2bytes 0x62 r1 r2
    Xorl   r1 r2   -> put_2bytes 0x63 r1 r2
    Jmp    label   -> put_5bytes 0x70 label
    Jle    label   -> put_5bytes 0x71 label
    Jl     label   -> put_5bytes 0x72 label
    Je     label   -> put_5bytes 0x73 label
    Jne    label   -> put_5bytes 0x74 label
    Jge    label   -> put_5bytes 0x75 label
    Jg     label   -> put_5bytes 0x76 label
    Cmovle r1 r2   -> put_2bytes 0x21 r1 r2
    Cmovl  r1 r2   -> put_2bytes 0x22 r1 r2
    Cmove  r1 r2   -> put_2bytes 0x23 r1 r2
    Cmovne r1 r2   -> put_2bytes 0x24 r1 r2
    Cmovge r1 r2   -> put_2bytes 0x25 r1 r2
    Cmovg  r1 r2   -> put_2bytes 0x26 r1 r2
    Call   label   -> put_5bytes 0x80 label
    Ret            -> putWord8 0x90
    Pushl  r1      -> put_2bytes 0xA0 r1 0xF
    Popl   r1      -> put_2bytes 0xB0 r1 0xF
  where
  f a b = shiftL a 4 .|. b

  put_word32          = putWord32le . fromIntegral
  put_label label     = put_word32 (fromJust $ lookup label mp)
  put_2bytes b1 r1 r2 = putWord8 b1 >> putWord8 (f r1 r2)
  put_5bytes b1 label = putWord8 b1 >> put_label label

putDirective :: Map Label Int -> String -> Int -> Put
putDirective mp s n = do
  addr <- get
  case s of
    "align" -> do
      let n_padding = n - addr `mod` n
      sequence_ $ replicate n_padding (putWord8 0)
    "byte"  -> putWord8 (fromIntegral n) >> put (addr + 1)
    "word"  -> putWord16le (fromIntegral n) >> put (addr + 2)
    "long"  -> putWord32le (fromIntegral n) >> put (addr + 4)
    _ -> return () -- TODO: "pos"?

putEntity :: Map Label Int -> Entity -> Put
putEntity mp e =
  case e of
    Instr (i, n) -> do
      addr <- get
      putInstr mp i
      put $ addr + n
    Directive (s, n) -> putDirective mp s n
    _ -> return ()

test :: String -> IO ()
test path = do
  s <- readFile path
  let xs  = runParser entities s
      mp  = genLabelMap xs
      put = sequence $ map (putEntity mp) xs
      ((_, builder), size) = runState (runWriterT put) 0
  putStrLn $ "Object file size: " ++ show size
  L.writeFile "a.out" $ toLazyByteString builder
