module Builtins where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import Evaluator
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

macroexpandAction :: LispFunction
macroexpandAction = LFAction "macroexpand" $ \vs ->
  case vs of
   [v] -> macroexpand v
   _   -> failWithString "macroexpand requires 1 value"

macroexpand1Action :: LispFunction
macroexpand1Action = LFAction "macroexpand-1" $ \vs ->
  case vs of
   [v] -> macroexpand1 v
   _   -> failWithString "macroexpand-1 requires 1 value"

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
                             ("gensym", LVFunction gensym),
                             ("macroexpand", LVFunction macroexpandAction),
                             ("macroexpand-1", LVFunction macroexpand1Action),
                             ("pi", LVNumber pi),
                             ("quit", LVFunction quit),
                             ("set-macro!", LVFunction setMacro)]

initEnv :: LispEnv
initEnv = LispEnv [] globalBuiltins 1 S.empty
