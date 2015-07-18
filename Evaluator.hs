module Evaluator where

import Types

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S

data LispState = Form LispValue | Value LispValue |
                 StateList [LispState] Int |
                 Apply LispFunction [LispValue] |
                 Special String [LispState]
               deriving Show


specialForms = S.Set String
specialForms = S.fromList ["def", "do", "if", "lambda", "quote"]

isSpecialForm :: LispValue -> Bool
isSpecialForm (LVList (LVSymbol str)) = str `S.elem` specialForms

resolveSymbol :: String -> [LispFrame] -> LispFrame -> Maybe LispValue
resolveSymbol str [] globals' = M.lookup str globals'
resolveSymbol str (frame:rest) globals' =
  case M.lookup str frame of
   Just val -> Just val
   Nothing  -> resolveSymbol str rest globals'

oneStep :: LispState -> Lisp LispState
oneStep v@(Value _) = return v

oneStep (Apply lispFn lispValues) =
  case lispFn of
    (LFPrimitive _ apply) ->
      case apply lispValues of
        Left  err -> lispFail err
        Right v   -> return $ Value v
    (LFAction _ action) ->
      Value `fmap` action lispValues
    (LFClosure _ _ _) -> error "TODO"

oneStep (Form (LVSymbol str)) = do
  theStack   <- use stack
  theGlobals <- use globals
  case resolveSymbol str theStack theGlobals of
   Just value -> return $ Value value
   Nothing    -> lispFail $ LispError (LVString $ "Can't resolve symbol: " ++ str)

-- empty list is "self-evaluating"
oneStep (Form (LVList [])) = return $ Value (LVList [])

oneStep f@(Form (LVList list)) =
  if isSpecialForm f
  then undefined
  else return $ StateList (map Form list) 0

oneStep (Form selfEval) = return $ Value selfEval

oneStep (StateList states n) =
  if n >= length states
  then case states of
        -- safe pattern match because n >= len --> all Value
        (Value (LVFunction f)):vals ->
          return $ Apply f (map (\(Value x) -> x) vals)
        _ -> lispFail $ LispError $ LVString "function required in application position"
  else case (states !! n) of
        Value _ -> return $ StateList states (n + 1)
        state'  -> do
          state1 <- oneStep state'
          return $ StateList ((take n states) ++ [state1] ++ (drop (n+1) states)) n

eval :: LispValue -> Lisp LispValue
eval lv =
  loop (Form lv)
  where loop state' =
          case state' of
           (Value v) -> return v
           _ -> oneStep state' >>= loop
