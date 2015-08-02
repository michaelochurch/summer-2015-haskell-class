{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Lisp.Printer where

import Control.Monad.IO.Class
import Data.List (intercalate)

import Lisp.Types

class LispShow a where
  lispShow :: a -> String

instance LispShow String where
  lispShow = show

instance LispShow LispValue where
  lispShow lispValue =
    case lispValue of
      LVString str      -> show str
      LVSymbol sym      -> sym
      LVNumber d        -> show d
      LVFunction primFn -> show primFn
      LVList lst        -> "(" ++ intercalate " " (map lispShow lst) ++ ")"
      LVBool True       -> "#t"
      LVBool False      -> "#f"

instance LispShow LispError where
  lispShow (LispError lispValue) =
    "{ !! ERROR : " ++ (lispShow lispValue) ++ "}"

lispPrint :: (MonadIO m, LispShow a) => a -> m ()
lispPrint = liftIO . putStrLn . lispShow
