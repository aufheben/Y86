module Main where

import Data.Monoid ((<>))
import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import LexC
import ParC
import SkelC
import PrintC
import AbsC

import ErrM

import qualified Data.Map as M

type ParseFun = [Token] -> Err Program

do_compile :: [External_declaration] -> IO ()
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

buildFuncTable :: [External_declaration] -> M.Map String External_declaration
buildFuncTable xs = build_func_table xs M.empty
  where
  build_func_table [] m = m
  build_func_table (decl:ext_decls) m =
    case decl of
      Afunc (NewFunc _ (NoPointer (NewFuncDec (Name (Ident name)) _)) _) ->
        let m' = M.insert name decl m
        in build_func_table ext_decls m'
      _ -> build_func_table ext_decls m

compile :: Program -> IO ()
compile (Progr ext_decls) = do
  -- build function table
  let func_table = buildFuncTable ext_decls
  putStrLn $ "# " <> show (M.size func_table) <> " functions defined"
  -- look for "main"
  case M.lookup "main" func_table of
    Just main_decl -> do
      putStrLn "main function found"
      do_compile ext_decls
    _ -> putStrLn "main function not defined"

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
