module Main where

import Control.Monad
import Control.Monad.Except
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import Builtins
import Evaluator
import Printer
import Reader
import Types

prompt :: String
prompt = "λισπ> "

flush :: IO ()
flush = hFlush stdout

tryRepl :: String -> Lisp LispValue
tryRepl input =
  case readString input of
   Left parseError -> throwError (LispError . LVString $ show parseError)
   Right lispValue -> eval lispValue

repl1 :: Lisp ()
repl1 = do liftIO $ putStr prompt
           liftIO flush
           inString <- liftIO getLine
           result   <- tryRepl inString
           liftIO $ lispPrint result

handleError :: LispError -> Lisp ()
handleError lispError = do
  liftIO $ lispPrint lispError
  purgeStack

repl :: Lisp ()
repl = forever (repl1 `catchError` handleError)

usingPrelude :: IO Bool
usingPrelude = fmap (notElem "--no-prelude") getArgs

main :: IO ()
main = do
  prelude <- usingPrelude
  env <- if prelude then withPreludeEnv else return initEnv
  repl `runLisp` env >> return ()
