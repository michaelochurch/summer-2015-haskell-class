module Builtins where

import Control.Lens
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import qualified Data.Set as S
import Evaluator
import Parser
import Printer
import Reader
import Types

dSum :: [Double] -> Double
dSum = sum

plus :: LispFunction
plus = liftFunction dSum Nothing "+"

dMinus :: [Double] -> Double
dMinus [] = 0
dMinus (d1:ds) = d1 - (sum ds)

minus :: LispFunction
minus = liftFunction dMinus Nothing "-"

dProd :: [Double] -> Double
dProd = product

times :: LispFunction
times = liftFunction dProd Nothing "*"

dDiv :: [Double] -> Double
dDiv [] = 1
dDiv (d1:ds) = d1 / (product ds)

divide :: LispFunction
divide = liftFunction dDiv Nothing "/"

dCmp :: (Double -> Double -> Bool) -> [Double] -> Bool
dCmp f [d1, d2] = d1 `f` d2
dCmp _ _ = error "arity error in dCmp"

numCmp :: (Double -> Double -> Bool) -> String -> LispValue
numCmp f str = LVFunction $ liftFunction (dCmp f) (Just 2) str

lispNot :: LispFunction
lispNot = LFPrimitive "not" $ \vs ->
  case vs of
   [(LVBool False)] -> Right $ LVBool True
   [_]              -> Right $ LVBool False
   _                -> Left $ LispError $ LVString "not requires one argument"

eq :: LispValue -> LispValue -> Bool
eq (LVString v1) (LVString v2) = v1 == v2
eq (LVSymbol v1) (LVSymbol v2) = v1 == v2
eq (LVNumber v1) (LVNumber v2) = v1 == v2
eq (LVBool   v1) (LVBool   v2) = v1 == v2
eq (LVList   l1) (LVList   l2) =
  (length l1 == length l2) && (and $ zipWith eq l1 l2)
eq _             _             = False

lispEq :: LispFunction
lispEq = LFPrimitive "eq" $ \vs ->
  case vs of
    [v1, v2] -> Right (LVBool $ eq v1 v2)
    _        -> Left  (LispError $ LVString "eq: requires two arguments")

atom :: LispValue -> Bool
atom (LVSymbol   _) = True
atom (LVString   _) = True
atom (LVNumber   _) = True
atom (LVBool     _) = True
atom (LVFunction _) = True
atom (LVList    []) = True
atom _              = False

lispAtom :: LispFunction
lispAtom = LFPrimitive "atom" $ \vs ->
  case vs of
   [v] -> Right $ LVBool $ atom v
   _   -> Left  $ LispError $ LVString "atom: requires one argument"

lispCar :: LispFunction
lispCar = LFPrimitive "car" $ \vs ->
  case vs of
   [(LVList (x:_))] -> Right x
   _                 -> Left $ LispError $ LVString "car: requires a non-empty list"

lispCdr :: LispFunction
lispCdr = LFPrimitive "cdr" $ \vs ->
  case vs of
   [(LVList (_:xs))] -> Right $ LVList xs
   _                 -> Left $ LispError $ LVString "cdr: requires a non-empty list"

lispCons :: LispFunction
lispCons = LFPrimitive "cons" $ \vs ->
  case vs of
   [x, LVList xs] -> Right $ LVList (x:xs)
   _              -> Left $ LispError $ LVString "cons: requires 2 args, 2nd must be list"

quit :: LispFunction
quit = LFAction "quit" $ \_ -> error "user quit"

gensym :: LispFunction
gensym = LFAction "gensym" $ \_ -> fmap LVSymbol genStr

setMacroAction :: [LispValue] -> Lisp LispValue
setMacroAction vals =
  case vals of
    [(LVSymbol name)] -> do
      macros . at name .= Just ()
      return $ LVBool True
    _                 -> failWithString "set-macro! requires one symbol"

setMacro :: LispFunction
setMacro = LFAction "set-macro!" $ setMacroAction

macroexpand1Action :: LispFunction
macroexpand1Action = LFAction "macroexpand-1" $ \vs ->
  case vs of
   [v] -> macroexpand1 v
   _   -> failWithString "macroexpand-1 requires 1 value"

macroexpandAction :: LispFunction
macroexpandAction = LFAction "macroexpand" $ \vs ->
  case vs of
   [v] -> macroexpand v
   _   -> failWithString "macroexpand requires 1 value"

macroexpandAllAction :: LispFunction
macroexpandAllAction = LFAction "macroexpand-all" $ \vs ->
  case vs of
   [v] -> macroexpandAll v
   _   -> failWithString "macroexpand-all requires 1 value"

printActionCore :: [LispValue] -> Lisp LispValue
printActionCore vs = do
  mapM_ (liftIO . lispPrint) vs
  case vs of
    [v] -> return v
    _   -> return $ LVList vs

printAction :: LispFunction
printAction = LFAction "print" printActionCore

execFile :: String -> Lisp LispValue
execFile filename = do
  parsedSExps <- liftIO $ loadSExps filename
  case parsedSExps of
   Left anError -> failWithString anError
   Right sexps  -> do
     mapM_ (eval . readSExp) sexps
     return $ LVBool True

loadFileAction :: LispFunction
loadFileAction = LFAction "load-file" $ \vs ->
  case vs of
   [(LVString filename)] -> execFile filename
   _                     -> failWithString "load-file: requires 1 string"

throwLispError :: LispFunction
throwLispError = LFAction "error" $ \vs ->
  case vs of
   [v] -> lispFail $ LispError v
   _   -> lispFail $ LispError $ LVList vs

globalBuiltins :: M.Map String LispValue
globalBuiltins = M.fromList [("+", LVFunction plus),
                             ("-", LVFunction minus),
                             ("*", LVFunction times),
                             ("/", LVFunction divide),
                             ("==", numCmp (==) "=="),
                             ("<=", numCmp (<=) "<="),
                             (">=", numCmp (>=) ">="),
                             ("/=", numCmp (/=) "/="),
                             ("<" , numCmp (<)  "<"),
                             (">" , numCmp (>)  ">"),
                             ("atom", LVFunction lispAtom),
                             ("car", LVFunction lispCar),
                             ("cdr", LVFunction lispCdr),
                             ("cons", LVFunction lispCons),
                             ("eq", LVFunction lispEq),
                             ("error", LVFunction throwLispError),
                             ("gensym", LVFunction gensym),
                             ("load-file", LVFunction loadFileAction),
                             ("macroexpand", LVFunction macroexpandAction),
                             ("macroexpand-1", LVFunction macroexpand1Action),
                             ("macroexpand-all", LVFunction macroexpandAllAction),
                             ("not", LVFunction lispNot),
                             ("pi", LVNumber pi),
                             ("print", LVFunction printAction),
                             ("quit", LVFunction quit),
                             ("set-macro!", LVFunction setMacro)]

initEnv :: LispEnv
initEnv = LispEnv [] globalBuiltins 1 S.empty
