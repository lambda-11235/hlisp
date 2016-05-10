
module Eval ( emptyEnv
            , initEnv
            , eval
            , evals)
       where

import LDatum
import Primitive

import Control.Monad.State
import qualified Data.Map as M


emptyEnv :: Env
emptyEnv = M.empty

initEnv :: Env
initEnv = (M.fromList [ ("cons", cons)
                      , ("car", car)
                      , ("cdr", cdr)
                      , ("=", eq)
                      , ("apply", apply)
                      , ("atom?", atomq)
                      , ("eval", lispEval)])



-- | Evaluates a lisp datum.
eval :: LDatum -> LispState
eval (Cons x xs) = handleApply x xs
eval f@(Function _ _ _) = return f
eval m@(Macro _ _ _) = return m
eval Nil = return Nil
eval (Symbol name) = handleLookup name



-- | Evaluates lisp data.
evals :: [LDatum] -> LispState
evals [] = return Nil
evals [x] = eval x
evals (x:xs) = do eval x
                  evals xs



-- | Looks up a value corresponding to a symbol in the environment.
handleLookup :: String -> LispState
handleLookup name =
    do env <- get
       case M.lookup name env of
        Just datum -> return datum
        Nothing -> fail $ "Unbounded symbol: " ++ name



-- | Handles an application of a special form or function.
handleApply :: LDatum -> LDatum -> LispState
handleApply (Symbol "if") args = lispIf args
handleApply (Symbol "label") args = label args
handleApply (Symbol "lambda") args = lambda args
handleApply (Symbol "macro") args = macro args
handleApply (Symbol "quote") args = quote args
handleApply (Symbol name) args = do datum <- handleLookup name
                                    case datum of
                                      f@(PrimFunc _) -> handleApply f args
                                      f@(Function _ _ _) -> handleApply f args
                                      m@(Macro _ _ _) -> handleApply m args
                                      _ -> fail $ "Non-function application: " ++ (show datum)
handleApply (PrimFunc f) args = applyPrimFunction f args
handleApply f@(Function _ _ _) args = applyFunction f args
handleApply m@(Macro _ _ _) args = applyMacro m args
handleApply Nil args = fail $ "Nil cannot be applied: " ++ (show args)
handleApply datum args = do datum' <- eval datum
                            handleApply datum' args



-- | Applies a Primitive function to a list of arguments.
applyPrimFunction :: (LDatum -> LispState) -> LDatum -> LispState
applyPrimFunction f args =
    do args' <- evalArgs args
       f args'
  where
    evalArgs Nil = return Nil
    evalArgs (Cons x xs) = do x' <- eval x
                              xs' <- evalArgs xs
                              return $ Cons x' xs'



-- | Applies a function to a list of arguments.
applyFunction :: LDatum -> LDatum -> LispState
applyFunction f@(Function vars body env) args =
    do args' <- evalArgs args
       applyGeneral f vars body env args'
  where
    evalArgs Nil = return Nil
    evalArgs (Cons x xs) = do x' <- eval x
                              xs' <- evalArgs xs
                              return $ Cons x' xs'



-- | Applies a macro to a list of arguments.
applyMacro :: LDatum -> LDatum -> LispState
applyMacro m@(Macro vars body env) args =
    do ret <- applyGeneral m vars body env args
       eval ret


-- | Does general application for both functions and macros. Does not evaluate
-- arguments or return values.
applyGeneral :: LDatum                   -- ^ The lisp datum being applied.
             -> (Either String [String]) -- ^ Its argument list.
             -> LDatum                   -- ^ Its executing body.
             -> Env                      -- ^ The environment it was created in.
             -> LDatum                   -- ^ The argumnents to apply.
             -> LispState
applyGeneral f vars body env args =
    do env' <- get
       put $ env
       remap vars args
       ret <- eval body
       put $ env'
       return ret
  where
    remap (Left var) args =
      do env <- get
         put $ M.insert var args env
         return Nil
    remap (Right vars) args = remapArgList vars args

    remapArgList (var:vars) (Cons arg args) =
      do env <- get
         put $ M.insert var arg env
         remapArgList vars args
    remapArgList (_:_) Nil =
      case vars of
       (Right vars) -> incorrectNumArgs (show f) args (length vars)
    remapArgList [] (Cons _ _) =
      case vars of
       (Right vars) -> incorrectNumArgs (show f) args (length vars)
    remapArgList [] Nil = return Nil



-- * Primitives

-- TODO: apply and lispEval should be defined in Primitive.hs.

apply = PrimFunc apply'
  where
    apply' (Cons f (Cons args Nil)) =
      case f of
       (Function vars body env) -> applyGeneral f vars body env args
       (PrimFunc f) -> f args
       x -> fail $ "Only functions can be applied: " ++ (show x)
    apply' args = incorrectNumArgs "apply" args 2



lispEval = PrimFunc lispEval'
  where
    lispEval' (Cons x Nil) = eval x
    lispEval' args = incorrectNumArgs "eval" args 1



-- * Special Forms

lispIf (Cons cond (Cons x (Cons y Nil))) =
    do cond' <- eval cond
       case cond' of
         Nil -> do y' <- eval y
                   return y'
         _ -> do x' <- eval x
                 return x'
lispIf args = incorrectNumArgs "if" args 3



label (Cons (Symbol name) (Cons x Nil)) =
    do env <- get
       x' <- eval x
       let x'' = case x' of
                  -- Needed for recursive functions
                  (Function args body env) -> Function args body (M.insert name x'' env)
                  (Macro args body env) -> Macro args body (M.insert name x'' env)
                  ld -> ld
       put $ M.insert name x'' env
       return Nil
label (Cons x (Cons _ Nil)) = fail $ "First argument to label should be a symbol: " ++ (show x)
label args = incorrectNumArgs "label" args 2



lambda = lambdaOrMacro Function "lambda"

macro = lambdaOrMacro Macro "macro"

lambdaOrMacro constructor name (Cons args (Cons body Nil)) =
    do env <- get
       case args of
        (Symbol var) -> return $ constructor (Left var) body env
        _ -> case getArgs args of
              Just args' -> return $ constructor (Right args') body env
              Nothing -> argsError args
  where
    getArgs (Cons (Symbol var) xs) = do vars <- getArgs xs
                                        return $ var:vars
    getArgs Nil = return []
    getArgs _ = Nothing
    argsError args = fail $ "All arguments should be symbols: " ++ (show args)

lambdaOrMacro _ name args = incorrectNumArgs name args 2



quote (Cons x Nil) = return x
quote args = incorrectNumArgs "quote" args 2
