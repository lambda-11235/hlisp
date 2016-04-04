{
module Lexer (Token (..), LexOut (..), scan) where
}

%wrapper "posn"

@nonspecial = [^\(\)'\.\32\t\r\n]

tokens :-

  $white+                               ;
  ";".*                                 ;
  "'"                                   { \p s -> lexOut p TQuote }
  "("                                   { \p s -> lexOut p TLParen }
  ")"                                   { \p s -> lexOut p TRParen }
  @nonspecial+                          { \p s -> lexOut p (TSymbol s) }

{
data Token = TQuote
           | TLParen
           | TRParen
           | TSymbol String
           deriving (Eq, Show)

data LexOut = LexOut { offset :: Int
                     , line :: Int
                     , column :: Int
                     , getToken :: Token }

lexOut (AlexPn offset line col) tok = LexOut offset line col tok

scan = alexScanTokens
}
