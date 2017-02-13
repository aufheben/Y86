{-# LANGUAGE TypeFamilies #-}

module Simulator.RAM where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Word
import Data.Serialize.Get
import Data.Serialize.Put
import Prelude hiding (putStrLn)
import Simulator.Util
import Simulator.Types
import Text.Printf
import qualified Data.Vector.Mutable as V
import qualified Data.ByteString as B

readBytes :: Integral a => a -> Int -> SimIO [Word8]
readBytes addr cnt = ask >>= rd (fromIntegral addr) cnt
  where
  rd _ 0 _   = return []
  rd i n ram = do
    b  <- liftIO $ V.read ram i
    bs <- rd (i + 1) (n - 1) ram
    return (b:bs)

writeBytes :: Integral a => a -> [Word8] -> SimIO ()
writeBytes addr bytes = ask >>= wr (fromIntegral addr) bytes
  where
  wr _ []     _   = return ()
  wr i (x:xs) ram = liftIO (V.write ram i x) >> wr (i + 1) xs ram

readWord32 :: Integral a => a -> SimIO Word32
readWord32 addr = do
  bs <- B.pack <$> readBytes addr 4
  let (Right w) = runGet getWord32le bs
  return w

writeWord32 :: Integral a => a -> Word32 -> SimIO ()
writeWord32 addr w = do
  let bytes = runPut (putWord32le w)
  writeBytes addr (B.unpack bytes)

clearRAM :: SimIO ()
clearRAM = do
  ram <- ask
  liftIO $ V.set ram 0

printRAM :: Integral a => a -> Int -> SimIO ()
printRAM addr cnt =
  readBytes addr cnt >>=
    putStrLn . unwords . map (printf "%02x")

----- utilities for decoding instructions

readRR _pc = decodeRegs . head <$> readBytes (_pc + 1) 1

readRRL _pc = do
  (ra, rb) <- readRR _pc
  w        <- readWord32 (_pc + 2)
  return (ra, rb, w)
