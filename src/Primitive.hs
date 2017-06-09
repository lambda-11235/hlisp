
module Primitive where

import LDatum

-- | Called whenever the incorrect number of arguments is given to something.
incorrectNumArgs :: String -- ^ The thing that was given the wrong number of args.
                 -> [LDatum] -- ^ A list of the arguments.
                 -> Int    -- ^ Expected number of arguments.
                 -> LispState
incorrectNumArgs name args num = fail $ "Incorrect number of arguments to "
                                 ++ name ++ " expacted (" ++ (show num) ++ "): "
                                 ++ (prettyPrint (List args))



cons = PrimFunc cons'
  where
    cons' [x, List xs] = return (List (x:xs))
    cons' [x, y] =  fail $ "cons should be called on a list, not " ++ (prettyPrint y)
    cons' args = incorrectNumArgs "cons" args 2



car = PrimFunc car'
  where
    car' [List (x:_)] = return x
    car' [List []] = fail $ "car applied to an empty list"
    car' [x] = fail $ "car should be called on a list, not " ++ (prettyPrint x)
    car' args = incorrectNumArgs "car" args 1



cdr = PrimFunc cdr'
  where
    cdr' [List (_:xs)] = return (List xs)
    cdr' [List []] = fail $ "cdr applied to an empty list"
    cdr' [x] = fail $ "cdr should be called on a list, not " ++ (prettyPrint x)
    cdr' args = incorrectNumArgs "cdr" args 1



eq = PrimFunc eq'
  where
    eq' [x, y] = if equal x y then return lispTrue else return lispFalse
    eq' args = incorrectNumArgs "eq" args 2



listq = PrimFunc listq'
  where
    listq' [List _] = return lispTrue
    listq' [_] = return lispFalse
    listq' args = incorrectNumArgs "list?" args 1
