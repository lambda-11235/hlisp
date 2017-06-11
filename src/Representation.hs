
module Representation where

import Control.Monad.State
import Data.IORef
import qualified Data.Map as M



type LispState = StateT Env IO

type Env = M.Map String Chunk

-- | A chunk is either an unevaluated piece of code along with its environment,
-- or a fully evaluated datum.
data Chunk = Chunk (IORef (Either (Env, AST) LDatum))

-- | Create a chunk from unevaluated code.
chunkUneval :: Env -> AST -> LispState Chunk
chunkUneval env expr = fmap Chunk $ lift $ newIORef (Left (env, expr))

-- | Create a chunk from an evaluated datum.
chunkValue :: LDatum -> LispState Chunk
chunkValue datum = fmap Chunk $ lift $ newIORef (Right datum)



data AST = SList [AST]
         | SSymbol String



-- | The lisp runtime data.
data LDatum = List [Chunk]
            | Symbol String
            | PrimFunc String [String] ([Chunk] -> LispState LDatum)
            -- | A function has a closure, an optional name to recurse on,
            -- some variables, and a body.
            | Function Env (Maybe String) (Either String [String]) AST


lispTrue :: LDatum
lispTrue = Function M.empty Nothing (Right ["x", "y"]) (SSymbol "x")

lispFalse :: LDatum
lispFalse = Function M.empty Nothing (Right ["x", "y"]) (SSymbol "y")
