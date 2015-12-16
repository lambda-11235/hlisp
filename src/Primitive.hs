
module Primitive where

import LDatum

-- | Called whenever the incorrect number of arguments is given to something.
incorrectNumArgs :: String -- ^ The thing that was given the wrong number of args.
                 -> LDatum -- ^ A list of the arguments.
                 -> Int    -- ^ Expected number of arguments.
                 -> LispState
incorrectNumArgs name args num = fail $ "Incorrect number of arguments to "
                                 ++ name ++ " expacted (" ++ (show num) ++ "): "
                                 ++ (show args)



cons = PrimFunc cons'
  where
    cons' (Cons a (Cons b Nil)) = return $ Cons a b
    cons' args = incorrectNumArgs "cons" args 2



car = PrimFunc car'
  where
    car' (Cons (Cons a b) Nil) = return a
    car' (Cons x Nil) = fail $ "car should be called on a cons cell, not " ++ (show x)
    car' args = incorrectNumArgs "car" args 1



cdr = PrimFunc cdr'
  where
    cdr' (Cons (Cons a b) Nil) = return b
    cdr' (Cons x Nil) = fail $ "cdr should be called on a cons cell, not " ++ (show x)
    cdr' args = incorrectNumArgs "cdr" args 1



eq = PrimFunc eq'
  where
    eq' (Cons a (Cons b Nil)) = if a == b then return lispTrue else return Nil
    eq' args = incorrectNumArgs "eq" args 2



atomq = PrimFunc atomq'
  where
    atomq' (Cons (Cons _ _) Nil) = return Nil
    atomq' (Cons _ Nil) = return lispTrue
    atomq' args = incorrectNumArgs "atomq" args 1
