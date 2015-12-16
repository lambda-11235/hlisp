
module LDatum where

import Data.List (intersperse)

import Data.Map (Map)

type Env = Map String LDatum



-- | The lisp data types. The function contains the names of its arguments, its
-- body, and the environment that it was created in. Same for macros.
data LDatum = Cons LDatum LDatum
            | Function (Either String [String]) LDatum Env
            | Macro (Either String [String]) LDatum Env
            | Nil
            | Symbol String
            deriving (Eq)



instance Show LDatum where
  show cc@(Cons _ _) = showCons cc
  show (Function (Left arg) body _) = "<lambda " ++ (show arg) ++ " ...>"
  show (Function (Right args) body _) = "<lambda (" ++ ((intersperse " " args) >>= id) ++ ") ...>"
  show (Macro (Left arg) body _) = "<macro " ++ (show arg) ++ " ...>"
  show (Macro (Right args) body _) = "<macro (" ++ ((intersperse " " args) >>= id) ++ ") ...>"
  show Nil = "()"
  show (Symbol name) = name

showCons :: LDatum -> String
showCons xs@(Cons _ (Cons _ _)) = '(' : showCons' xs
  where
    showCons' (Cons x xs@(Cons _ _)) = (show x) ++ " " ++ (showCons' xs)
    showCons' (Cons x Nil) = (show x) ++ ")"
    showCons' (Cons x y) = "(" ++ (show x) ++ " . " ++ (show y) ++ "))"
showCons (Cons x Nil) = "(" ++ (show x) ++ ")"
showCons (Cons x y) = "(" ++ (show x) ++ " . " ++ (show y) ++ ")"



-- | The true symbol for HLisp-Min.
lispTrue :: LDatum
lispTrue = Symbol "t"



-- | Convenience quoting function.
quoteDatum :: LDatum -> LDatum
quoteDatum x = (Cons (Symbol "quote") (Cons x Nil))



-- | Turns a Haskell list into a list lisp.
lispListify :: [LDatum] -> LDatum
lispListify [] = Nil
lispListify (x:xs) = Cons x (lispListify xs)
