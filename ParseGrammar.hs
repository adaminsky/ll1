module ParseGrammar where

import Text.ParserCombinators.Parsec
import System.Environment
import Data.Char
import FirstFollow

{-
This is the simple grammar for representing grammars. The grammar itself almost
represents itself, but I am using some helpful constructs like the '+' to mean
one or more, and '*' to mean zero or more.

<Grammar> -> +(<Rule>)
<Rule> -> <NT> -> <SF> *(| <SF>)
<SF> -> <SF><SF> | <NT> | <T>
<NT> -> [A-Z]
<T> -> [a-z]
-}

nonterminal :: Parser Node
nonterminal = do
  nt <- oneOf ['A'..'Z']
  return $ N $ [toUpper nt]

terminal :: Parser Node
terminal = do
  t <- oneOf ['a'..'z']
  return $ T $ [toLower t]

parseSF :: Parser [Node]
parseSF = many1 (nonterminal <|> terminal)

parseRHSLast :: Parser [Node]
parseRHSLast = do
  char '|'
  spaces
  r <- parseSF
  return r

parseRHS :: Parser [[Node]]
parseRHS = do
  first <- parseSF
  spaces
  rest <- option [] $ many1 $ parseRHSLast
  return $ first : rest

parseRule :: Parser Rule
parseRule = do
  lhs <- nonterminal
  spaces
  char '-'
  char '>'
  spaces
  rhs <- parseRHS
  newline
  return $ Rule{lhs=lhs, rhs=rhs}

parseGrammar :: Parser Grammar
parseGrammar = do
  g <- many1 parseRule
  return $ Grammar g
