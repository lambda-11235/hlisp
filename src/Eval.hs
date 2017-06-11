
module Eval ( emptyEnv
            , initEnv
            , eval
            , evals
            , prettyPrint)
       where

import Representation

import Control.Monad.State
import Data.List (intersperse)
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
             capply <- newIORef (Right apply)
             return (M.fromList [ ("cons", Chunk ccons)
                                , ("car", Chunk ccar)
                                , ("cdr", Chunk ccdr)
                                , ("=", Chunk ceq)
                                , ("list?", Chunk clistq)
                                , ("apply", Chunk capply)])


force :: Chunk -> LispState LDatum
force (Chunk ref) = do x <- lift $ readIORef ref
                       case x of
                         Left (env, expr) ->
                           do oldEnv <- get
                              put env
                              datum <- eval expr
                              put oldEnv
                              lift $ writeIORef ref (Right datum)
                              return datum

                         Right datum -> return datum


data Error = UnboundSym String
           | NonFunApplied LDatum [Chunk]
           | IncorrectNumArgs Int [Chunk]
           | PrimIncorrectNumArgs String Int [Chunk]
           | PrimCalledOnNonList String LDatum
           | PrimCalledOnEmptyList String
           | SpecialIncorrectNumArgs String [AST]

-- | FIXME: We should return a Either, but I don't have access to the ST
-- transformer right now.
failWith :: Error -> LispState a
failWith (UnboundSym name) = fail $ "Unbound symbol " ++ name
failWith (NonFunApplied f xs) =
  do f' <- prettyPrint f
     xs' <- prettyPrint (List xs)
     fail $ "Non-function " ++ f' ++ " applied to arguments " ++ xs'
failWith (IncorrectNumArgs num args) =
  do args' <- prettyPrint (List args)
     fail $ "Expected " ++ show num ++ " arguments, got " ++ args'
failWith (PrimIncorrectNumArgs name num args) =
  do args' <- prettyPrint (List args)
     fail $ name ++ " expected " ++ show num ++ " arguments, got " ++ args'
failWith (PrimCalledOnNonList name x) =
  do x' <- prettyPrint x
     fail $ name ++ " called on non-list " ++ x'
failWith (PrimCalledOnEmptyList name) = fail $ name ++ " called on an empty list"
failWith (SpecialIncorrectNumArgs name args) =
  fail $ name ++ " has a bad number of arguments: " ++ prettyPrintAST (SList args)


prettyPrint :: LDatum -> LispState String
prettyPrint (List xs) =
  do xs' <- mapM force xs
     xs'' <- mapM prettyPrint xs'
     return $ "(" ++ ((intersperse " " xs'') >>= id) ++ ")"
prettyPrint (Symbol name) = return name
prettyPrint (Function _ _ (Left arg) _) = return $ "<lambda " ++ (show arg) ++ " ...>"
prettyPrint (Function _ _ (Right args) _) =
  return $ "<lambda (" ++ ((intersperse " " args) >>= id) ++ ") ...>"
prettyPrint (PrimFunc _ args _) =
  return $ "<lambda (" ++ ((intersperse " " args) >>= id) ++ ") ...>"

prettyPrintAST :: AST -> String
prettyPrintAST (SList xs) =
  "(" ++ ((intersperse " " (map prettyPrintAST xs)) >>= id) ++ ")"
prettyPrintAST (SSymbol name) = name



-- | Evaluates a lisp datum.
eval :: AST -> LispState LDatum
eval (SList ((SSymbol "label"):xs)) = label xs
eval (SList ((SSymbol "lambda"):xs)) = lambda xs
eval (SList ((SSymbol "quote"):xs)) = quote xs
eval (SList (x:xs)) =
  do env <- get
     x' <- eval x
     xs' <- mapM (chunkUneval env) xs
     handleApply x' xs'
eval (SList []) = return (List [])
eval (SSymbol name) = handleLookup name



-- | Evaluates lisp data.
evals :: [AST] -> LispState ()
evals [] = return ()
evals (x:xs) = do eval x
                  evals xs



-- | Looks up a value corresponding to a symbol in the environment.
handleLookup :: String -> LispState LDatum
handleLookup name =
    do env <- get
       case M.lookup name env of
         Just chunk -> force chunk
         Nothing -> failWith (UnboundSym name)



-- | Handles an application of a special form or function.
handleApply :: LDatum -> [Chunk] -> LispState LDatum
handleApply (PrimFunc name vars f) args = applyPrimFunction name vars f args
handleApply f@(Function env name vars body) args =
  applyFunction f env name vars body args
handleApply x args = failWith (NonFunApplied x args)



-- | Applies a Primitive function to a list of arguments.
applyPrimFunction :: String -> [String] -> ([Chunk] -> LispState LDatum)
                  -> [Chunk] -> LispState LDatum
applyPrimFunction name vars f args =
  if length vars /= length args then
    failWith (PrimIncorrectNumArgs name (length vars) args)
  else
    f args



-- | Applies a function to a list of arguments.
applyFunction :: LDatum -> Env -> Maybe String -> Either String [String]
              -> AST -> [Chunk] -> LispState LDatum
applyFunction f env name vars body args =
  do oldEnv <- get
     case name of
       Nothing -> put env
       Just str -> do chunk <- chunkValue f
                      put (M.insert str chunk env)
     case vars of
       Left var -> do chunk <- chunkValue (List args)
                      modify (M.insert var chunk)
       Right vs -> mapArgs oldEnv vs args
     res <- eval body
     put oldEnv
     return res
  where
    mapArgs oldEnv vars args =
      if length vars /= length args then
        failWith $ IncorrectNumArgs (length vars) args
      else
        zipWithM_ mapArg vars args

    mapArg :: String -> Chunk -> LispState ()
    mapArg var arg = modify (M.insert var arg)



-- * Primitive functions.

cons = PrimFunc "cons" ["x", "xs"] cons'
  where
    cons' [x, xs] =
      do xs' <- force xs
         case xs' of
           List xs'' -> return (List (x:xs''))
           y -> failWith (PrimCalledOnNonList "cons" y)
    cons' args = failWith (PrimIncorrectNumArgs "cons" 2 args)



car = PrimFunc "car" ["xs"] car'
  where
    car' [xs] =
      do xs' <- force xs
         case xs' of
           List (x:_) -> force x
           List _ -> failWith (PrimCalledOnEmptyList "car")
           x -> failWith (PrimCalledOnNonList "car" x)
    car' args = failWith (PrimIncorrectNumArgs "car" 1 args)



cdr = PrimFunc "cdr" ["xs"] cdr'
  where
    cdr' [xs] =
      do xs' <- force xs
         case xs' of
           List (_:xs) -> return (List xs)
           List _ -> failWith (PrimCalledOnEmptyList "cdr")
           x -> failWith (PrimCalledOnNonList "cdr" x)
    cdr' args = failWith (PrimIncorrectNumArgs "cdr" 1 args)



eq = PrimFunc "=" ["x", "y"] eq'
  where
    eq' [x, y] = do x' <- force x
                    y' <- force y
                    isEq <- equal x' y'
                    if isEq then return lispTrue else return lispFalse
    eq' args = failWith (PrimIncorrectNumArgs "=" 2 args)

    equal (List []) (List []) = return True
    equal (List _) (List []) = return False
    equal (List []) (List _) = return False
    equal (List (x:xs)) (List (y:ys)) =
      do x' <- force x
         y' <- force y
         isEq <- equal x' y'
         if isEq then equal (List xs) (List ys) else return False
    equal (Symbol x) (Symbol y) = return (x == y)
    equal _ _ = return False



listq = PrimFunc "list?" ["x"] listq'
  where
    listq' [xs] =
      do xs' <- force xs
         case xs' of
           List _ -> return lispTrue
           _ -> return lispFalse
    listq' args = failWith (PrimIncorrectNumArgs "list?" 1 args)



apply = PrimFunc "apply" ["f", "xs"] apply'
  where
    apply' [f, xs] =
      do f' <- force f
         xs' <- force xs
         case xs' of
           List xs'' -> handleApply f' xs''
           x -> failWith (PrimCalledOnNonList "apply" x)
    apply' args = failWith (PrimIncorrectNumArgs "apply" 2 args)



-- * Special Forms

label :: [AST] -> LispState LDatum
label [SSymbol name, x] =
    do env <- get
       chunk <- chunkUneval env x
       put $ M.insert name chunk env
       return (List [])
label [x, _] = fail $ "First argument to label should be a symbol: " ++ prettyPrintAST x
label args = failWith (SpecialIncorrectNumArgs "label" args)



lambda :: [AST] -> LispState LDatum
lambda [(SSymbol args), body] =
  do env <- get
     return (Function env Nothing (Left args) body)
lambda [(SSymbol name), (SSymbol args), body] =
  do env <- get
     return (Function env (Just name) (Left args) body)
lambda [(SList args), body] =
  do env <- get
     strArgs <- strArgs args
     return (Function env Nothing (Right strArgs) body)
lambda [(SSymbol name), (SList args), body] =
  do env <- get
     strArgs <- strArgs args
     return (Function env (Just name) (Right strArgs) body)
lambda [x, _, _] = fail $ "lambda expected symbol, not " ++ prettyPrintAST x
lambda args = failWith (SpecialIncorrectNumArgs "Lambda" args)

strArgs [] = return []
strArgs ((SSymbol name):xs) = do xs' <- strArgs xs
                                 return (name:xs')
strArgs (x:_) = fail $ "Expected symbol got " ++ prettyPrintAST x



quote :: [AST] -> LispState LDatum
quote [SList xs] = do xs' <-  mapM (\x -> quote [x]) xs
                      xs'' <- mapM chunkValue xs'
                      return (List xs'')
quote [SSymbol name] = return (Symbol name)
quote args = failWith (SpecialIncorrectNumArgs "quote" args)
