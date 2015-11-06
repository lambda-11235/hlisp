
import Eval
import Parse

import Control.Monad.State
import qualified Data.Map as M
import System.Environment (getArgs)
import System.IO

main = do args <- getArgs
          envs <- evalFiles args emptyEnvs
          repl envs

evalFiles :: [String] -> Envs -> IO Envs
evalFiles [] envs = return envs
evalFiles (file:files) envs =
    do code <- readFile file
       case lispParse code of
         Left err -> do print err
                        evalFiles files envs
         Right xs -> do (x', envs') <- runStateT (evals xs) envs
                        evalFiles files envs'
       

repl envs = do hPutStr stdout "> "
               hFlush stdout
               line <- getLine
               case lispParseDatum line of
                 Left err -> print err
                 Right x -> do (x', envs') <- runStateT (eval x) envs
                               print x'
                               repl envs'
