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

type ParseFun = [Token] -> Err Program

do_compile [] = return ()
do_compile (decl:ext_decls) = do
  case decl of
    Afunc (OldFunc decl_specifiers declarator decs compound_stm) ->
      putStrLn "Afunc OldFunc"
    Afunc (NewFunc decl_specifiers declarator compound_stm) ->
      putStrLn "Afunc NewFunc"
    Afunc (OldFuncInt declarator decs compound_stm) ->
      putStrLn "Afunc OldFuncInt"
    Afunc (NewFuncInt declarator compound_stm) ->
      putStrLn "Afunc NewFuncInt"
    Global (NoDeclarator decl_specifiers) ->
      putStrLn "Global NoDeclarator"
    Global (Declarators decl_specifiers init_declarator) ->
      putStrLn "Global Declarators"
  do_compile ext_decls

compile :: Program -> IO ()
compile (Progr ext_decls) = do_compile ext_decls

runFile :: ParseFun -> FilePath -> IO ()
runFile p f = putStrLn f >> readFile f >>= run p

run :: ParseFun -> String -> IO ()
run p s = let ts = myLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrLn "Tokens:"
                          putStrLn $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showTree tree
                          compile tree
                          exitSuccess

showTree :: Program -> IO ()
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
