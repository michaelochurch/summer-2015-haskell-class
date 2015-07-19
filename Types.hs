{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S

data LispValue =
  LVString String | LVSymbol String | LVNumber Double |
  LVBool Bool | LVList [LispValue] | LVFunction LispFunction deriving Show

isNumber :: LispValue -> Bool
isNumber (LVNumber _) = True
isNumber _            = False

type Lisp a = ExceptT LispError (StateT LispEnv IO) a

data LispFunction =
  LFPrimitive {lfpName :: String,
               lfpApply :: [LispValue] -> Either LispError LispValue} |
  LFAction    {lfaName :: String,
               lfaApply :: [LispValue] -> Lisp LispValue} |
  LFClosure   {lfcName :: String,
               lfcStack :: [LispFrame],
               lfcParams :: [String],
               lfcBody :: LispValue}

instance Show LispFunction where
  show (LFPrimitive name _) = "< prim. fn named " ++ name ++ " >"
  show (LFAction    name _) = "< prim. action named " ++ name ++ " >"
  show (LFClosure   name _ _ _) = "< closure called " ++ name ++ " >"

newtype LispError = LispError LispValue deriving Show

invalidArg :: String -> [LispValue] -> LispError
invalidArg name vals =
  LispError $ LVString $ "Invalid argument: " ++ name ++ "; " ++ (intercalate " " $ map show vals) ++ ")"

type LispFrame = M.Map String LispValue

data LispEnv = LispEnv {_stack         :: [LispFrame],
                        _globals       :: LispFrame,
                        _gensymCounter :: Integer,
                        _macros        :: S.Set String} deriving Show

makeLenses ''LispEnv

runLisp :: Lisp a -> LispEnv -> IO (Either LispError a, LispEnv)
runLisp lispAction env =
  (runExceptT lispAction) `runStateT` env

lispFail :: LispError -> Lisp a
lispFail le = ExceptT $ StateT $ \s -> return (Left le, s)

failWithString :: String -> Lisp a
failWithString = lispFail . LispError . LVString

emptyEnv :: LispEnv
emptyEnv = LispEnv [] M.empty 1 S.empty

tryLisp :: Lisp a -> IO (Either LispError a, LispEnv)
tryLisp act = act `runLisp` emptyEnv

class Liftable a where
  toLisp :: a -> LispValue
  fromLisp :: LispValue -> Maybe a

instance Liftable Double where
  toLisp = LVNumber
  fromLisp (LVNumber x) = Just x
  fromLisp _           = Nothing

instance Liftable Bool where
  toLisp = LVBool
  fromLisp (LVBool x) = Just x
  fromLisp _         = Nothing

-- TODO: add arity checking (for functions like not, eq, etc.)
liftFunction :: (Liftable a, Liftable b) => ([a] -> b) -> Maybe Int -> String -> LispFunction
liftFunction f arity name = LFPrimitive name f1
  where f1 xs =
          case mapM fromLisp xs of
            Just vs ->
              if arityCheck vs
              then Right $ toLisp $ f vs
              else Left $ invalidArg name xs
            Nothing -> Left $ invalidArg name xs
        arityCheck xs =
          case arity of
           Nothing -> True
           Just n  -> length xs == n


genStr :: Lisp String
genStr = do
  n <- use gensymCounter
  gensymCounter += 1
  return $ "G__" ++ (show n)

-- Right now, we don't have pattern matching in lambdas so we assume an LVList
-- of LVSymbols.
paramNames :: LispValue -> Maybe [String]
paramNames (LVList syms) =
  forM syms $ \s ->
    case s of
      LVSymbol str -> Just str
      _            -> Nothing

paramNames _ = Nothing

mkClosure :: String -> LispValue -> LispValue -> Lisp LispFunction
mkClosure name paramList body = do
  stack' <- use stack
  case paramNames paramList of
    Just params -> return $ LFClosure name stack' params body
    Nothing    -> failWithString "mkClosure : given bad parameter list"

pushFrame :: LispFrame -> Lisp ()
pushFrame frame =
  modify $ stack %~ (frame:)

pushFrames :: [LispFrame] -> Lisp Int
pushFrames frames = do
  forM_ frames pushFrame
  return $ length frames

popFrame :: Lisp ()
popFrame =
  modify $ stack %~ tail

popFrames :: Int -> Lisp ()
popFrames n = replicateM_ n popFrame
