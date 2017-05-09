module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import LexC
import ParC
import SkelC
import PrintC
import AbsC

import ErrM

type ParseFun a = [Token] -> Err a

runFile :: (Print a, Show a) => ParseFun a -> FilePath -> IO ()
runFile p f = putStrLn f >> readFile f >>= run p

run :: (Print a, Show a) => ParseFun a -> String -> IO ()
run p s = let ts = myLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrLn "Tokens:"
                          putStrLn $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showTree tree
                          exitSuccess

showTree :: (Show a, Print a) => a -> IO ()
showTree tree = do
  putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (files)         Compile standalone C files"
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    fs -> mapM_ (runFile pProgram) fs
