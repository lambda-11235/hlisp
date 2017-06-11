
import Eval
import Parse
import Lexer
import Representation

import Control.Monad.State
import qualified Data.Map as M
import qualified System.Console.Readline as RL
import System.Environment (getArgs)
import System.IO
import System.IO.Error

main = do args <- getArgs
          env <- initEnv
          envs <- evalFiles args env
          repl envs

evalFiles :: [String] -> Env -> IO Env
evalFiles [] envs = return envs
evalFiles (file:files) envs =
    do code <- readFile file
       case lispParse objs file $ scan code of
         Left err -> do print err
                        evalFiles files envs
         Right xs -> do ((), envs') <- runStateT (evals xs) envs
                        evalFiles files envs'


repl envs = do line <- RL.readline "> "
               case line of
                 Nothing -> return ()

                 Just str ->
                   do RL.addHistory str
                      envs' <- tryIOError $ handleLine str envs
                      case envs' of
                        Left err -> do print err
                                       repl envs
                        Right envs'' -> repl envs''

handleLine line envs =
  case lispParse obj "REPL" (scan line) of
    Left err -> fail (show err)
    Right x -> do (x', envs') <- runStateT (eval x >>= prettyPrint) envs
                  putStrLn x'
                  return envs'
