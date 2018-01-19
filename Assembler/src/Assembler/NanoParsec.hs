module Assembler.NanoParsec where

import Data.Char
import Control.Applicative
import Control.Monad

newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Functor Parser where
  fmap f (Parser p) = Parser (\s -> [(f a, b) | (a, b) <- p s])

instance Applicative Parser where
  pure a = Parser (\s -> [(a, s)])
  (Parser p1) <*> (Parser p2) =
    Parser (\s -> [(f a, s2) | (f, s1) <- p1 s, (a, s2) <- p2 s1])

instance Monad Parser where
  return   = pure
  p >>= f  = Parser (\s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s)

-- 'and' semantics
instance MonadPlus Parser where
  mzero     = Parser (\_ -> [])
  mplus p q = Parser (\s -> parse p s ++ parse q s)

-- 'or' semantics
instance Alternative Parser where
  empty   = mzero
  p <|> q = Parser $ \s ->
    case parse p s of
      []     -> parse q s
      res    -> res

-- from Alternative we get `many` (zero or more) and `some` (one or more)
-- some, many :: Parser a -> Parser [a]

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, "")] -> res
    [(_, _)]    -> error "Parser did not consume entire stream"
    _           -> error "Parser error"

item :: Parser Char
item = Parser $ \s ->
  case s of
   []     -> []
   (c:cs) -> [(c,cs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- item
  if p c then return c else mzero

-----

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

char :: Char -> Parser Char
char c = satisfy (c ==)

string :: String -> Parser String
string [] = return []
string (c:cs) = do
  char c
  string cs
  return (c:cs)

spaces :: Parser String
spaces = many $ satisfy isSpace

----- token utils

token :: Parser a -> Parser a
token p = do
  a <- p
  spaces
  return a

word :: Parser String
word = token (some $ satisfy isAlphaNum)

number :: Parser Int
number = token $ do
  s <- string "-" <|> return ""
  cs <- some (satisfy isAlphaNum)
  return $ read (s ++ cs)

-- c must not be "AlphaNum"
endsWith :: Char -> Parser String
endsWith c = token $ do
  s <- some (satisfy isAlphaNum)
  char c
  return s
