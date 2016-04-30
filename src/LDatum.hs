
module LDatum where

import Control.Monad.State (StateT)
import Data.List (intersperse)
import Data.Map (Map)



type LispState = StateT Env IO LDatum

type Env = Map String LDatum


-- | The lisp data types. The function contains the names of its arguments, its
-- body, and the environment that it was created in. Same for macros.
data LDatum = Cons LDatum LDatum
            | PrimFunc (LDatum -> LispState)
            | Function (Either String [String]) LDatum Env
            | Macro (Either String [String]) LDatum Env
            | Nil
            | Symbol String



instance Eq LDatum where
  (==) (Cons a b) (Cons c d) = (a == c) && (b == d)
  (==) (PrimFunc _) (PrimFunc _) = False
  (==) (Function a1 b1 e1) (Function a2 b2 e2) =
    (a1 == a2) && (b1 == b1) && (e1 == e2)
  (==) (Macro a1 b1 e1) (Macro a2 b2 e2) =
    (a1 == a2) && (b1 == b1) && (e1 == e2)
  (==) Nil Nil = True
  (==) (Symbol a) (Symbol b) = a == b
  (==) _ _ = False



instance Show LDatum where
  show cc@(Cons _ _) = showCons cc
  show (PrimFunc _) = "<primitive function>"
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
