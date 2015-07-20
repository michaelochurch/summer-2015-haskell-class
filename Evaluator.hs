module Evaluator where

import Types

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S

data LispState = Form LispValue | Value LispValue |
                 StateList [LispState] Int |
                 Apply LispFunction [LispValue] |
                 InsideClosure LispState Int |
                 Special String [LispState]
               deriving Show

specialForms :: S.Set String
specialForms = S.fromList ["def", "do", "if", "lambda", "quote"]

specialFormCheck :: LispValue -> Maybe (String, [LispValue])
specialFormCheck (LVList ((LVSymbol str):rest)) =
  if str `S.member` specialForms
  then Just (str, rest)
  else Nothing
specialFormCheck _ = Nothing

(!!?) :: [a] -> Int -> Maybe a
(!!?) list n =
  if n < 0 || n >= (length list)
  then Nothing
  else Just (list !! n)

resolveSymbol :: String -> [LispFrame] -> LispFrame -> Maybe LispValue
resolveSymbol str [] globals' = M.lookup str globals'
resolveSymbol str (frame:rest) globals' =
  case M.lookup str frame of
   Just val -> Just val
   Nothing  -> resolveSymbol str rest globals'

truthy :: LispValue -> Bool
truthy (LVBool False) = False
truthy _              = True

defineSymbol :: String -> LispValue -> Lisp ()
defineSymbol str value =
  globals %= M.insert str value

undefineSymbol :: String -> Lisp ()
undefineSymbol str =
  globals %= M.delete str

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
    s@(LFClosure name stack' params body) -> do
      let self       = LVFunction s
          --TODO: need to handle &rest case intelligently.
          newFrame   = M.fromList $ zip (name:params) (self:lispValues)
          newStack   = newFrame:stack'
      nFrames <- pushFrames (newFrame:newStack)
      return $ InsideClosure (Form body) nFrames

oneStep (InsideClosure value@(Value _) nFrames) = do
  popFrames nFrames
  return $ value

oneStep (InsideClosure state nFrames) = do
  state' <- oneStep state
  -- TODO: if the closure is a tail call, TCO can be done here.
  return $ InsideClosure state' nFrames

oneStep (Form (LVSymbol str)) = do
  theStack   <- use stack
  theGlobals <- use globals
  case resolveSymbol str theStack theGlobals of
   Just value -> return $ Value value
   Nothing    -> lispFail $ LispError (LVString $ "Can't resolve symbol: " ++ str)

-- empty list is "self-evaluating"
oneStep (Form (LVList [])) = return $ Value (LVList [])

oneStep (Form form@(LVList list)) =
  case specialFormCheck form of
    Just (string, rest) ->
      return $ Special string (map Form rest)
    Nothing -> return $ StateList (map Form list) 0

oneStep (Form selfEval) = return $ Value selfEval

oneStep (Special "quote" [(Form val)]) =
  return $ Value val

oneStep (Special "quote" _) =
  failWithString "quote : requires one form"

oneStep (Special "if" [(Value x), thenForm, elseForm]) =
  return $ if truthy x then thenForm else elseForm

oneStep (Special "if" [condState, thenForm, elseForm]) = do
  condState1 <- oneStep condState
  return $ Special "if" [condState1, thenForm, elseForm]

oneStep (Special "if" _) = failWithString  "if : requires 3 forms"

oneStep (Special "def" [(Form (LVSymbol str)), (Value x)]) = do
  defineSymbol str x
  return $ Value $ LVBool True

oneStep (Special "def" [name, defState]) = do
  defState1 <- oneStep defState
  return $ Special "def" [name, defState1]

oneStep (Special "def" _) = failWithString "def : requires a name (symbol) and 1 form"

oneStep (Special "do" []) = return $ Value $ LVBool True

oneStep (Special "do" ((Value x):[])) = return $ Value x

oneStep (Special "do" ((Value _):rest)) = return $ Special "do" rest

oneStep (Special "do" (state:rest)) = do
  state1 <- oneStep state
  return $ Special "do" (state1:rest)

oneStep (Special "lambda" [(Form (LVSymbol name)), (Form params), (Form body)]) = do
  closure <- mkClosure name params body
  return . Value . LVFunction $ closure

oneStep (Special "lambda" [(Form params), (Form body)]) = do
  name <- genStr
  closure <- mkClosure name params body
  return . Value . LVFunction $ closure

oneStep (Special "lambda" _) = failWithString "lambda : requires 2 or 3 forms"

oneStep (Special name _) = error $ "illegal state : unknown special form " ++ name

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
