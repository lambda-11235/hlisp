{-|
Module: Parse

The module to parse lisp expressions.
-}

module Parse (lispParse, obj, objs) where

import LDatum
import Lexer

import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim


type Parser = Parsec [LexOut] ()


match :: Token -> Parser ()
match tok = tokenPrim (show . getToken) pos (match' . getToken)
  where
    match' x = if x == tok then Just () else Nothing

symbol :: Parser LDatum
symbol = tokenPrim (show . getToken) pos (match' . getToken)
  where
    match' (TSymbol sym) = Just $ Symbol sym
    match' _ = Nothing

pos :: (SourcePos -> LexOut -> [LexOut] -> SourcePos)
pos oldPos (LexOut _ line col _) _ = newPos (sourceName oldPos) line col


-- | Parses a string into a series of list objects.
lispParse :: Parser a -> String -> [LexOut] -> Either ParseError a
lispParse p name toks = runParser p () name toks


-- | A parser for one lisp object.
obj :: Parser LDatum
obj = list <|> quote <|> symbol

-- | A parser for multiple lisp object.
objs :: Parser [LDatum]
objs = do o <- obj
          os <- option [] objs
          return (o : os)

list :: Parser LDatum
list = do match TLParen
          os <- option [] objs
          match TRParen
          return $ lispListify os

quote :: Parser LDatum
quote = do match TQuote
           l <- obj
           return $ quoteDatum l
