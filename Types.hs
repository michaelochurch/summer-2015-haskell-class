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

data LispFunction =
  LFPrimitive {lfpName :: String,
               lfpApply :: [LispValue] -> Either LispError LispValue}

instance Show LispFunction where
  show (LFPrimitive name _) = "< prim. fn named " ++ name ++ " >"

newtype LispError = LispError LispValue deriving Show

invalidArg :: String -> [LispValue] -> LispError
invalidArg name vals =
  LispError $ LVString $ "Invalid argument: " ++ name ++ "; " ++ (intercalate " " $ map show vals) ++ ")"

data LispState = Form LispValue | Value LispValue |
                 StateList [LispState] Int |
                 Apply LispFunction [LispValue] deriving Show

type LispFrame = M.Map String LispValue

data LispEnv = LispEnv {_stack         :: [LispFrame],
                        _globals       :: LispFrame,
                        _gensymCounter :: Integer,
                        _macros        :: S.Set String} deriving Show

makeLenses ''LispEnv

type Lisp a = ExceptT LispError (StateT LispEnv IO) a

runLisp :: Lisp a -> LispEnv -> IO (Either LispError a, LispEnv)
runLisp lispAction env =
  (runExceptT lispAction) `runStateT` env

lispFail :: LispError -> Lisp a
lispFail le = ExceptT $ StateT $ \s -> return (Left le, s)

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

liftFunction :: (Liftable a, Liftable b) => ([a] -> b) -> String -> LispFunction
liftFunction f name = LFPrimitive name f1
  where f1 xs =
          case mapM fromLisp xs of
            Just vs -> Right $ toLisp $ f vs
            Nothing -> Left $ invalidArg name xs
