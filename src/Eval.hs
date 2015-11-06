
module Eval where

import LDatum

import Control.Monad.State
import qualified Data.Map as M

-- | Represents both the global and local environments.
data Envs = Envs { globalEnv :: Env
                 , localEnv :: Env
                 }

emptyEnvs :: Envs
emptyEnvs = Envs M.empty M.empty



type LispState = StateT Envs IO LDatum



-- | Called whenever the incorrect number of arguments is given to something.
incorrectNumArgs :: String -- ^ The thing that was given the wrong number of args.
                 -> LDatum -- ^ A list of the arguments.
                 -> LispState
incorrectNumArgs name args = fail $ "Incorrect number of arguments to " ++ name
                                    ++ ": " ++ (show args)



-- | Evaluates a lisp datum.
eval :: LDatum -> LispState
eval (Cons x xs) = handleApply x xs
eval f@(Function _ _ _) = return f
eval m@(Macro _ _ _) = return m
eval Nil = return Nil
eval (Symbol name) = handleLookup name



-- | Evaluates a lisp data.
evals :: [LDatum] -> LispState
evals [] = return Nil
evals [x] = eval x
evals (x:xs) = do eval x
                  evals xs



-- | Looks up the value in a symbol first in the local environment and then the
-- global one.
handleLookup :: String -> LispState
handleLookup name =
    do (Envs genv lenv) <- get
       case M.lookup name lenv of
         Just datum -> return datum
         Nothing -> case M.lookup name genv of
                      Just datum -> return datum
                      _ -> fail $ "Unbounded symbol: " ++ name



-- | Handles an application of a special form or function.
handleApply :: LDatum -> LDatum -> LispState
handleApply (Symbol "cons") args = cons args
handleApply (Symbol "car") args = car args
handleApply (Symbol "cdr") args = cdr args
handleApply (Symbol "=") args = eq args
handleApply (Symbol "atom?") args = atomq args
handleApply (Symbol "eval") args = lispEval args
handleApply (Symbol "if") args = lispIf args
handleApply (Symbol "label") args = label args
handleApply (Symbol "lambda") args = lambda args
handleApply (Symbol "macro") args = macro args
handleApply (Symbol "quote") args = quote args
handleApply (Symbol name) args = do datum <- handleLookup name
                                    case datum of
                                      f@(Function _ _ _) -> handleApply f args
                                      m@(Macro _ _ _) -> handleApply m args
                                      _ -> fail $ "Non-function application: " ++ (show datum)
handleApply f@(Function _ _ _) args = applyFunction f args
handleApply m@(Macro _ _ _) args = applyMacro m args
handleApply Nil args = fail $ "Nil cannot be applied: " ++ (show args)
handleApply datum args = do datum' <- eval datum
                            handleApply datum' args



-- | Applies a function to a list of arguments.
applyFunction :: LDatum -> LDatum -> LispState
applyFunction f@(Function vars body flenv) args =
    do (Envs genv lenv) <- get
       put $ Envs genv (M.union flenv lenv)
       remap vars args
       ret <- eval body
       (Envs genv _) <- get
       put $ Envs genv lenv
       return ret
  where
    remap (var:vars) (Cons arg args) =
      do arg' <- eval arg
         (Envs genv lenv) <- get
         put $ Envs genv (M.insert var arg' lenv)
         remap vars args
    remap (_:_) Nil = incorrectNumArgs (show f) args
    remap [] (Cons _ _) = incorrectNumArgs (show f) args
    remap [] Nil = return Nil



-- NOTE: This function differ from applyFunction on evaluation strategy.
-- | Applies a macro to a list of arguments.
applyMacro :: LDatum -> LDatum -> LispState
applyMacro m@(Macro vars body flenv) args =
    do (Envs genv lenv) <- get
       put $ Envs genv (M.union flenv lenv)
       remap vars args
       ret <- eval body
       (Envs genv _) <- get
       put $ Envs genv lenv
       eval ret
  where
    remap (var:vars) (Cons arg args) =
      do (Envs genv lenv) <- get
         put $ Envs genv (M.insert var arg lenv)
         remap vars args
    remap (_:_) Nil = incorrectNumArgs (show m) args
    remap [] (Cons _ _) = incorrectNumArgs (show m) args
    remap [] Nil = return Nil



-- * Primitives

cons (Cons x (Cons xs Nil)) = do x' <- eval x
                                 xs' <- eval xs
                                 return $ Cons x' xs'
cons args = incorrectNumArgs "cons" args



car (Cons x Nil) =
    do x' <- eval x
       case x' of
         (Cons y ys) -> return y
         _ -> fail $ "Argument to car must be cons cell: " ++ (show x)
car args = incorrectNumArgs "car" args



cdr (Cons x Nil) =
    do x' <- eval x
       case x' of
         (Cons y ys) -> return ys
         _ -> fail $ "Argument to cdr must be cons cell: " ++ (show x)
cdr args = incorrectNumArgs "cdr" args



eq (Cons x (Cons y Nil)) = do x' <- eval x
                              y' <- eval y
                              if x' == y' then
                                return lispTrue
                              else
                                return $ Nil
eq args = incorrectNumArgs "=" args



atomq (Cons x Nil) = do x' <- eval x
                        case x' of
                          (Cons _ _) -> return Nil
                          _ -> return lispTrue
atomq args = incorrectNumArgs "atom?" args



lispEval (Cons x Nil) = do x' <- eval x
                           eval x'
lispEval args = incorrectNumArgs "eval" args



lispIf (Cons cond (Cons x (Cons y Nil))) =
    do cond' <- eval cond
       case cond' of
         Nil -> do y' <- eval y
                   return y'
         _ -> do x' <- eval x
                 return x'
lispIf args = incorrectNumArgs "if" args



label (Cons (Symbol name) (Cons x Nil)) =
    do x' <- eval x
       (Envs genv lenv) <- get
       put $ Envs (M.insert name x' genv) lenv
       return Nil
label (Cons x (Cons _ Nil)) = fail $ "First argument to label should be a symbol: " ++ (show x)
label args = incorrectNumArgs "label" args



lambda (Cons args (Cons body Nil)) =
    do (Envs _ lenv) <- get
       case getArgs args of
         Just args' -> return $ Function args' body lenv
         Nothing -> argsError args
  where
    getArgs (Cons (Symbol var) xs) = do vars <- getArgs xs
                                        return $ var:vars
    getArgs Nil = return []
    getArgs _ = Nothing
    argsError args = fail $ "All arguments should be symbols: " ++ (show args)

lambda args = incorrectNumArgs "lambda" args



macro (Cons args (Cons body Nil)) =
    do (Envs _ lenv) <- get
       case getArgs args of
         Just args' -> return $ Macro args' body lenv
         Nothing -> argsError args
  where
    getArgs (Cons (Symbol var) xs) = do vars <- getArgs xs
                                        return $ var:vars
    getArgs Nil = return []
    getArgs _ = Nothing
    argsError args = fail $ "All arguments should be symbols: " ++ (show args)

macro args = incorrectNumArgs "macro" args



quote (Cons x Nil) = return x
quote args = incorrectNumArgs "quote" args
