{-# LANGUAGE TypeFamilies #-}

module Simulator.RAM where

import Data.Word
import Data.Serialize.Get
import Data.Serialize.Put
import Simulator.Util
import qualified Data.Vector.Mutable as V
import qualified Data.ByteString as B

readBytes :: Integral a => V.IOVector Word8 -> a -> Int -> IO [Word8]
readBytes ram addr cnt = rd (fromIntegral addr) cnt
  where
  rd _ 0 = return []
  rd i n = do
    b  <- V.read ram i
    bs <- rd (i + 1) (n - 1)
    return (b:bs)

writeBytes :: Integral a => V.IOVector Word8 -> a -> [Word8] -> IO ()
writeBytes ram addr bytes = wr (fromIntegral addr) bytes
  where
  wr _ []     = return ()
  wr i (x:xs) = V.write ram i x >> wr (i + 1) xs

readWord32 :: Integral a => V.IOVector Word8 -> a -> IO Word32
readWord32 ram addr = do
  bs <- B.pack <$> readBytes ram addr 4
  let (Right w) = runGet getWord32le bs
  return w

writeWord32 :: Integral a => V.IOVector Word8 -> a -> Word32 -> IO ()
writeWord32 ram addr w = do
  let bytes = runPut (putWord32le w)
  writeBytes ram addr (B.unpack bytes)

----- utilities for decoding instructions

readRR ram _pc = decodeRegs . head <$> readBytes ram (_pc + 1) 1

readRRL ram _pc = do
  (ra, rb) <- readRR ram _pc
  w        <- readWord32 ram (_pc + 2)
  return (ra, rb, w)
