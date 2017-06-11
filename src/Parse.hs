{-|
Module: Parse

The module to parse lisp expressions.
-}

module Parse (lispParse, obj, objs) where

import Lexer
import Representation

import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim


type Parser = Parsec [LexOut] ()


match :: Token -> Parser ()
match tok = tokenPrim (show . getToken) pos (match' . getToken)
  where
    match' x = if x == tok then Just () else Nothing

symbol :: Parser String
symbol = tokenPrim (show . getToken) pos (match' . getToken)
  where
    match' (TSymbol sym) = Just sym
    match' _ = Nothing

pos :: (SourcePos -> LexOut -> [LexOut] -> SourcePos)
pos oldPos (LexOut _ line col _) _ = newPos (sourceName oldPos) line col


-- | Parses a string into a series of list objects.
lispParse :: Parser a -> String -> [LexOut] -> Either ParseError a
lispParse p name toks = runParser (p <* eof) () name toks


-- | A parser for one lisp object.
obj :: Parser AST
obj = list <|> quote <|> (fmap SSymbol symbol)

-- | A parser for multiple lisp object.
objs :: Parser [AST]
objs = many1 obj

list :: Parser AST
list = do match TLParen
          os <- many obj
          match TRParen
          return (SList os)

quote :: Parser AST
quote = do match TQuote
           l <- obj
           return (SList [SSymbol "quote", l])
