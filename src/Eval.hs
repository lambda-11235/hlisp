
module Eval ( emptyEnv
            , initEnv
            , eval
            , evals)
       where

import LDatum
import Primitive

import Control.Monad.State
import Data.IORef
import qualified Data.Map as M


emptyEnv :: Env
emptyEnv = M.empty

initEnv :: IO Env
initEnv = do ccons <- newIORef (Right cons)
             ccar <- newIORef (Right car)
             ccdr <- newIORef (Right cdr)
             ceq <- newIORef (Right eq)
             clistq <- newIORef (Right listq)
             return (M.fromList [ ("cons", Chunk ccons)
                                , ("car", Chunk ccar)
                                , ("cdr", Chunk ccdr)
                                , ("=", Chunk ceq)
                                , ("list?", Chunk clistq)])



-- | Evaluates a lisp datum.
eval :: LDatum -> LispState
eval (List (x:xs)) = handleApply x xs
eval (List []) = return (List [])
eval (Symbol name) = handleLookup name
eval f@(PrimFunc _) = return f
eval f@(Function _ _ _ _) = return f



-- | Evaluates lisp data.
evals :: [LDatum] -> LispState
evals [] = return (List [])
evals [x] = eval x
evals (x:xs) = do eval x
                  evals xs



-- | Looks up a value corresponding to a symbol in the environment.
handleLookup :: String -> LispState
handleLookup name =
    do oldEnv <- get
       case M.lookup name oldEnv of
         Just (Chunk ref) ->
           do x <- liftIO $ readIORef ref
              case x of
                Left (env, expr) ->
                  do put env
                     datum <- eval expr
                     put oldEnv
                     liftIO $ writeIORef ref (Right datum)
                     return datum
                Right datum -> return datum
         Nothing -> fail $ "Unbounded symbol: " ++ name



-- | Handles an application of a special form or function.
handleApply :: LDatum -> [LDatum] -> LispState
handleApply (Symbol "label") args = label args
handleApply (Symbol "lambda") args = lambda args
handleApply (Symbol "quote") args = quote args
handleApply (Symbol "apply") args = apply args
handleApply (Symbol name) args =
  do datum <- handleLookup name
     case datum of
       f@(PrimFunc _) -> handleApply f args
       f@(Function _ _ _ _) -> handleApply f args
       _ -> fail $ "Non-function application: " ++ (prettyPrint datum)
handleApply (PrimFunc f) args = applyPrimFunction f args
handleApply f@(Function env name vars body) args =
  applyFunction f env name vars body args
handleApply (List []) args = fail $ "An empty list cannot be applied"
handleApply datum args = do datum' <- eval datum
                            handleApply datum' args



-- | Applies a Primitive function to a list of arguments.
applyPrimFunction :: ([LDatum] -> LispState) -> [LDatum] -> LispState
applyPrimFunction f args =
  do args' <- mapM eval args
     f args'



-- | Applies a function to a list of arguments.
applyFunction :: LDatum -> Env -> Maybe String -> Either String [String]
              -> LDatum -> [LDatum] -> LispState
applyFunction f env name vars body args =
  do oldEnv <- get
     case name of
       Nothing -> put env
       Just str -> do ref <- liftIO $ newIORef (Right f)
                      put (M.insert str (Chunk ref) env)
     case vars of
       Left var -> do ref <- liftIO $ newIORef (Left (oldEnv, listify args))
                      modify (M.insert var (Chunk ref))
       Right vs -> mapArgs oldEnv vs args
     res <- eval body
     put oldEnv
     return res
  where
    mapArgs oldEnv vars args =
      if length vars /= length args then
        fail $ "Wrong number of args " ++ (show $ length args) ++ " expected " ++ (show $ length vars)
      else
        zipWithM_ (mapArg oldEnv) vars args

    mapArg :: Env -> String -> LDatum -> LispState
    mapArg oldEnv var arg = do ref <- liftIO $ newIORef (Left (oldEnv, arg))
                               modify (M.insert var (Chunk ref))
                               return (List [])

    -- | Here we turn the argument list into a form that will evaluate to a
    -- list.
    listify :: [LDatum] -> LDatum
    listify [] = List []
    listify (x:xs) = (List [Symbol "cons", x, listify xs])



-- * Special Forms

label :: [LDatum] -> LispState
label [Symbol name, x] =
    do env <- get
       ref <- liftIO $ newIORef (Left (env, x))
       put $ M.insert name (Chunk ref) env
       return (List [])
label [x, _] = fail $ "First argument to label should be a symbol: " ++ (prettyPrint x)
label args = incorrectNumArgs "label" args 2



lambda :: [LDatum] -> LispState
lambda [(Symbol args), body] =
  do env <- get
     return (Function env Nothing (Left args) body)
lambda [(Symbol name), (Symbol args), body] =
  do env <- get
     return (Function env (Just name) (Left args) body)
lambda [(List args), body] =
  do env <- get
     strArgs <- strArgs args
     return (Function env Nothing (Right strArgs) body)
lambda [(Symbol name), (List args), body] =
  do env <- get
     strArgs <- strArgs args
     return (Function env (Just name) (Right strArgs) body)
lambda [x, _] = fail $ "lambda expected arguments, not " ++ prettyPrint x
lambda [x, _, _] = fail $ "lambda expected symbol, not " ++ prettyPrint x
lambda args = incorrectNumArgs "lambda" args 2

strArgs [] = return []
strArgs ((Symbol name):xs) = do xs' <- strArgs xs
                                return (name:xs')
strArgs (x:_) = fail $ "Expected symbol got " ++ (prettyPrint x)



quote :: [LDatum] -> LispState
quote [x] = return x
quote args = incorrectNumArgs "quote" args 1



apply :: [LDatum] -> LispState
apply [f, xs] = do xs' <- eval xs
                   case xs' of
                     -- We have to quote each element so it won't be
                     -- reevaluated.
                     List xs'' -> handleApply f (map quoteDatum xs'')
                     x -> fail $ "apply's second argument must be a list, not " ++ (prettyPrint x)
apply args = incorrectNumArgs "apply" args 2
