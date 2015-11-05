
module LDatum where

import Data.List (intersperse)

data LDatum = Cons LDatum LDatum
            | Function [String] LDatum
            | Nil
            | Symbol String
            deriving (Eq)

instance Show LDatum where
  show cc@(Cons _ _) = showCons cc
  show (Function args body) = "<lambda (" ++ ((intersperse " " args) >>= id) ++ ") ...>"
  show Nil = "()"
  show (Symbol name) = name

showCons :: LDatum -> String
showCons xs@(Cons _ (Cons _ _)) = '(' : showCons' xs
  where
    showCons' (Cons x xs@(Cons _ _)) = (show x) ++ " " ++ (showCons' xs)
    showCons' (Cons x Nil) = (show x) ++ ")"
    showCons' (Cons x y) = "(" ++ (show x) ++ " . " ++ (show y) ++ "))"
showCons (Cons x y) = "(" ++ (show x) ++ " . " ++ (show y) ++ ")"

lispTrue :: LDatum
lispTrue = Symbol "true"

quoteDatum :: LDatum -> LDatum
quoteDatum x = (Cons (Symbol "quote") (Cons x Nil))

lispListify :: [LDatum] -> LDatum
lispListify [] = Nil
lispListify (x:xs) = Cons x (lispListify xs)
