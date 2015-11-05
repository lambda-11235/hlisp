
import Eval
import Parse

import Control.Monad.State
import qualified Data.Map as M
import System.IO

main = repl M.empty

repl vars = do hPutStr stdout "> "
               hFlush stdout
               line <- getLine
               case lispParseDatum line of
                 Left err -> print err
                 Right x -> do (x', vars') <- runStateT (eval x) vars
                               print x'
                               repl vars'
