{-|
Module: Parse

The module to parse lisp expressions.
-}

module Parse (lispParse, lispParseDatum) where

import LDatum

import Text.ParserCombinators.Parsec

-- | Parses a string into a list object.
lispParseDatum :: String -> Either ParseError LDatum
lispParseDatum = (parse obj "HLisp Object") . strip

-- | Parses a string into a series of list objects.
lispParse :: String -> Either ParseError [LDatum]
lispParse = (parse objs "HLisp Objects") . strip

-- | Strips whitespace from string.
strip :: String -> String
strip str = reverse $ dropWhile (`elem` " \t\n\r") $ reverse
            $ dropWhile (`elem` " \t\n\r") str

-- | A parser for one lisp object.
obj :: Parser LDatum
obj = list <|> quote <|> symbol

-- | A parser for multiple lisp object.
objs :: Parser [LDatum]
objs = do skipable
          o <- obj
          os <- option [] objs
          skipable
          return (o : os)

list :: Parser LDatum
list = do char '('
          os <- option [] objs
          char ')'
          return $ lispListify os

quote :: Parser LDatum
quote = do char '\''
           skipable
           l <- obj
           return $ quoteDatum l

skipable :: Parser ()
skipable = skipMany (space <|> comment)

comment :: Parser Char
comment = do char ';'
             manyTill anyChar newline
             return ';'

symbol :: Parser LDatum
symbol = do name <- many1 nonspecial
            return $ Symbol name

nonspecial :: Parser Char
nonspecial = oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "!@#$%^&*{}[]|?/\\,.<>;:_-+=")

special :: Parser Char
special = oneOf "()'\""
