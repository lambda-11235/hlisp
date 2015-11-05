
module Eval where

import LDatum

import Control.Monad.State
import qualified Data.Map as M

type LispState = StateT (M.Map String LDatum) IO LDatum



incorrectNumArgs :: String -> LDatum -> LispState
incorrectNumArgs name args = fail $ "Incorrect number of arguments to " ++ name
                                    ++ ": " ++ (show args)



eval :: LDatum -> LispState
eval (Cons x xs) = handleApply x xs
eval f@(Function _ _) = return f
eval Nil = return Nil
eval (Symbol name) = handleLookup name



handleLookup :: String -> LispState
handleLookup name = do vars <- get
                       case M.lookup name vars of
                         Just datum -> return datum
                         Nothing -> fail $ "Unbounded symbol: " ++ name



handleApply :: LDatum -> LDatum -> LispState
handleApply (Symbol "cons") args = cons args
handleApply (Symbol "car") args = car args
handleApply (Symbol "cdr") args = cdr args
handleApply (Symbol "=") args = eq args
handleApply (Symbol "atom?") args = atomq args
handleApply (Symbol "if") args = lispIf args
handleApply (Symbol "label") args = label args
handleApply (Symbol "lambda") args = lambda args
handleApply (Symbol "quote") args = quote args
handleApply (Symbol name) args = do datum <- handleLookup name
                                    case datum of
                                      f@(Function _ _) -> handleApply f args
                                      _ -> fail $ "Non-function application: " ++ (show datum)
handleApply f@(Function _ _) args = applyFunction f args
handleApply datum args = do datum' <- eval datum
                            handleApply datum' args
--handleApply datum _ = fail $ "Non-function application: " ++ (show datum)



applyFunction :: LDatum -> LDatum -> LispState
applyFunction (Function vars body) args =
    do oldVars <- get
       remap vars args
       ret <- eval body
       put oldVars
       return ret
  where
    remap (var:vars) (Cons arg args) =
      do arg' <- eval arg
         m <- get
         put $ M.insert var arg' m
         remap vars args
    remap (_:_) Nil = incorrectNumArgs "<function>" args
    remap [] (Cons _ _) = incorrectNumArgs "<function>" args
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
                              if x == y then
                                return lispTrue
                              else
                                return $ Nil
eq args = incorrectNumArgs "=" args



atomq (Cons x Nil) = do x' <- eval x
                        case x' of
                          (Cons _ _) -> return Nil
                          _ -> return lispTrue
atomq args = incorrectNumArgs "atom?" args



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
       vars <- get
       put $ M.insert name x' vars
       return Nil
label (Cons x (Cons _ Nil)) = fail $ "First argument to label should be a symbol: " ++ (show x)
label args = incorrectNumArgs "label" args



lambda (Cons args (Cons body Nil)) = case getArgs args of
                                       Just args' -> return $ Function args' body
                                       Nothing -> argsError args
  where
    getArgs (Cons (Symbol var) xs) = do vars <- getArgs xs
                                        return $ var:vars
    getArgs Nil = return []
    getArgs _ = Nothing
    argsError args = fail $ "All arguments should be symbols: " ++ (show args)

lambda args = incorrectNumArgs "lambda" args



quote (Cons x Nil) = return x
quote args = incorrectNumArgs "quote" args
