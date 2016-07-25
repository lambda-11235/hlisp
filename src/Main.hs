
import Eval
import Parse
import LDatum
import Lexer

import Control.Monad.State
import qualified Data.Map as M
import System.Environment (getArgs)
import System.IO
import System.IO.Error

main = do args <- getArgs
          envs <- evalFiles args initEnv
          repl envs

evalFiles :: [String] -> Env -> IO Env
evalFiles [] envs = return envs
evalFiles (file:files) envs =
    do code <- readFile file
       case lispParse objs file $ scan code of
         Left err -> do print err
                        evalFiles files envs
         Right xs -> do (x', envs') <- runStateT (evals xs) envs
                        evalFiles files envs'


repl envs = do hPutStr stdout "> "
               hFlush stdout
               line <- getLine
               envs' <- tryIOError $ handleLine line envs
               case envs' of
                 Left err -> do print err
                                repl envs
                 Right envs'' -> repl envs''

handleLine line envs = case lispParse obj "REPL" $ scan line of
                         Left err -> fail $ show err
                         Right x -> do (x', envs') <- runStateT (eval x) envs
                                       print x'
                                       return envs'
