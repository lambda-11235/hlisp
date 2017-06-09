
module LDatum ( LispState
              , Env
              , Chunk (..)
              , LDatum (..)
              , lispTrue
              , lispFalse
              , quoteDatum
              , equal
              , prettyPrint)
       where

import Control.Monad.State (StateT)
import Data.List (intersperse)
import Data.IORef
import qualified Data.Map as M



type LispState = StateT Env IO LDatum

type Env = M.Map String Chunk

-- | A chunk is like a closure over a single piece of data.
data Chunk = Chunk (IORef (Either (Env, LDatum) LDatum))


-- | The lisp data types. The function contains the names of its arguments, its
-- body, and the environment that it was created in (its closure).
data LDatum = List [LDatum]
            | Symbol String
            | PrimFunc ([LDatum] -> LispState)
            | Function Env (Maybe String) (Either String [String]) LDatum


lispTrue :: LDatum
lispTrue = Function M.empty Nothing (Right ["x", "y"]) (Symbol "x")

lispFalse :: LDatum
lispFalse = Function M.empty Nothing (Right ["x", "y"]) (Symbol "y")

quoteDatum :: LDatum -> LDatum
quoteDatum x = List [Symbol "quote", x]



equal :: LDatum -> LDatum -> Bool
equal (List xs) (List ys) = (length xs == length ys) && (all id (zipWith equal xs ys))
equal (Symbol x) (Symbol y) = x == y
equal _ _ = False



prettyPrint :: LDatum -> String
prettyPrint (List xs) = "(" ++ ((intersperse " " (map prettyPrint xs)) >>= id) ++ ")"
prettyPrint (Symbol name) = name
prettyPrint (Function _ _ (Left arg) body) = "<lambda " ++ (show arg) ++ " ...>"
prettyPrint (Function _ _ (Right args) body) = "<lambda (" ++ ((intersperse " " args) >>= id) ++ ") ...>"
prettyPrint (PrimFunc _) = "<primitive function>"
