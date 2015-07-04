{-# LANGUAGE TemplateHaskell #-}

-- Lisp design goals

-- eval,
--  and, or as macros
-- list, of course.

-- def, defun, defmacro
-- apply

-- load a file that defines factorial and map and range. use at repl.
-- Lambda calculus / Church numeral demonstration.
-- none of this needs to be tail recursive or fast.

module Main where

import Control.Applicative
import Control.Lens hiding (noneOf)
import Control.Monad.State
import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import qualified Data.Map as M
import System.IO (hFlush, stdout)
import Text.Parsec hiding (many, (<|>))

data SExp = SAtom String | SList [SExp] deriving (Eq, Show)

parseAtom :: Parsec String u SExp
parseAtom = SAtom <$> many1 (satisfy (\c -> c `notElem` "()\"" && (not . isSpace) c))

inStringLit :: Parsec String u Char
inStringLit = (noneOf "\"\\") <|> string "\\\"" *> return '"' <|> string "\\\\"
 *> return '\\'

stringLit :: Parsec String u String
stringLit = between (char '"') (char '"') (many inStringLit)

enquote :: String -> String
enquote s = "\"" ++ s ++ "\""

parseString :: Parsec String u SExp
parseString = (SAtom . enquote) <$> stringLit

parseList :: Parsec String u SExp
parseList = SList <$> (char '(' *> many space *> body <* many space <* char ')')
  where body = sepBy parseSExp (many1 space)

quoteAtom :: SExp
quoteAtom = SAtom "quote"

parseQuoted :: Parsec String u SExp
parseQuoted = (\x -> SList [quoteAtom, x]) <$> (char '\'' *> parseSExp)

parseSExp :: Parsec String u SExp
parseSExp = parseQuoted <|> parseList <|> parseString <|> parseAtom




data PrimFn = PrimFn {pfName :: String, unPrimFn :: ([LispValue] -> LispValue)}

instance Show PrimFn where
  show (PrimFn name _) = "< prim. function called " ++ name ++ " >"

type LispFrame = M.Map String LispValue


data LispClosure = LispClosure {lcName   :: Maybe LispValue,
                                lcParams :: [LispValue],
                                lcStack  :: [LispFrame],
                                lcBody   :: LispValue} deriving Show

data LispValue = LString String | LSymbol String | LNumber Double |
                 LFunction PrimFn |
                 LClosure LispClosure |
                 LList [LispValue] | LBool Bool | LError String |
                 LMacro String LispValue deriving Show

lispShow :: LispValue -> String
lispShow lispValue =
  case lispValue of
    LString str      -> show str
    LSymbol sym      -> sym
    LNumber d        -> show d
    LFunction primFn -> show primFn
    LList lst        -> "(" ++ intercalate " " (map lispShow lst) ++ ")"
    LBool True       -> "#t"
    LBool False      -> "#f"
    LClosure (LispClosure maybeName params _stack' body) ->
      "< " ++ name ++ "λ " ++ (lispShow . LList $ params) ++ " . " ++ (lispShow body) ++ " >"
      where name = case maybeName of
                      Just sym -> (lispShow sym) ++ " = "
                      Nothing -> ""
    LError str       -> "< ERROR: " ++ str ++ " >"
    LMacro name _    -> "< macro called " ++ name ++ ">"

dequoteStringLiteral :: String -> String
dequoteStringLiteral s =
  if length s >= 2 && (head s) == '"' && (last s) == '"'
  then tail . init $ s
  else error "dequoteStringLiteral : requires a string literal"

readSAtom :: String -> LispValue
readSAtom ""     = LString ""
readSAtom s@(c:_) =
  case c of
    '\"' -> LString $ dequoteStringLiteral s
    '#'  -> case s of
              "#t" -> LBool True
              "#f" -> LBool False
              _    -> LSymbol s
    '-'  -> if s == "-" || (not . isDigit) (s !! 1)
            then LSymbol s
            else LNumber (read s)
    d | isDigit d  -> LNumber (read s)
    _              -> LSymbol s

readSExp :: SExp -> LispValue
readSExp (SAtom s) = readSAtom s
readSExp (SList l) = LList $ map readSExp l

readString :: String -> LispValue
readString s =
  case parse parseSExp "" s of
    Left e     -> LError $ "Parse Error: " ++ (show e)
    Right sExp -> readSExp sExp

-- To make the lifting of functions to LispValue fn's easier.
class Liftable a where
  toLisp :: a -> LispValue
  fromLisp :: LispValue -> Maybe a

instance Liftable Double where
  toLisp = LNumber
  fromLisp (LNumber x) = Just x
  fromLisp _           = Nothing

instance Liftable Bool where
  toLisp = LBool
  fromLisp (LBool x) = Just x
  fromLisp _         = Nothing

invalidArg :: String -> [LispValue] -> LispValue
invalidArg name vals =
  LError $ "Invalid Argument Error: (" ++ name ++ " " ++ (intercalate " " $ map lispShow vals) ++ ")"

liftToLisp1 :: (Liftable a, Liftable b) => (a -> b) -> String -> LispValue
liftToLisp1 f name = LFunction (PrimFn name f1)
  where f1 xs@[x1] =
          case fromLisp x1 of
            Just v1 -> toLisp $ f v1
            _       -> invalidArg name xs
        f1 xs = invalidArg name xs

liftToLisp2 :: (Liftable a, Liftable b, Liftable c) => (a -> b -> c) -> String -> LispValue
liftToLisp2 f name = LFunction (PrimFn name f2)
  where f2 xs@[x1, x2] =
          case (fromLisp x1, fromLisp x2) of
          (Just v1, Just v2) -> toLisp $ f v1 v2
          _                  -> invalidArg name xs
        f2 xs = invalidArg name xs

selfEvaluating :: LispValue -> Bool
selfEvaluating (LList [])  = True
selfEvaluating (LList _)   = False
selfEvaluating (LSymbol _) = False
selfEvaluating _           = True

headIs :: LispValue -> String -> Bool
headIs lv str =
  case lv of
    (LList ((LSymbol sym):_)) -> str == sym
    _                         -> False

getPos :: LispValue -> Int -> LispValue
getPos (LList xs) n = xs !! n
getPos _          _ = error "getPos : requires an LList"

getStr :: LispValue -> String
getStr (LSymbol str) = str
getStr _             = error "getStr : requires an LSymbol"

data RuntimeState = RuntimeState {_globalBindings :: LispFrame,
                                  _verbose        :: Bool,
                                  _stack          :: [LispFrame],
                                  _gensymCounter  :: Integer} deriving Show

makeLenses ''RuntimeState

type Lisp a = StateT RuntimeState IO a

runLisp :: Lisp a -> RuntimeState -> IO (a, RuntimeState)
runLisp lispAction rt = lispAction `runStateT` rt

defineSymbol :: LispValue -> LispValue -> Lisp ()
defineSymbol symbol value =
  globalBindings %= M.insert (getStr symbol) value

undefSymbol :: LispValue -> Lisp ()
undefSymbol symbol =
  globalBindings %= M.delete (getStr symbol)

isError :: LispValue -> Bool
isError (LError _) = True
isError _          = False

truthy :: LispValue -> Bool
truthy (LBool False) = False
truthy (LError _)    = error "giving errors truth values is a bad idea."
truthy _             = True

isSymbol :: LispValue -> Bool
isSymbol (LSymbol _) = True
isSymbol _           = False

isListForm :: LispValue -> Bool
isListForm (LList _) = True
isListForm _         = False

evalIf :: LispValue -> LispValue -> LispValue -> Lisp LispValue
evalIf condForm thenForm elseForm =
  do
    condVal <- eval condForm
    if isError condVal
    then return condVal
    else if truthy condVal
      then eval thenForm
      else eval elseForm

evalDef :: LispValue -> LispValue -> Lisp LispValue
evalDef symbol valForm =
  do
    undefSymbol symbol -- avoid what is being def'd capturing a stale version of itself
    value <- eval valForm
    defineSymbol symbol value
    return $ LBool True

evalSetVerbose :: LispValue -> Lisp LispValue
evalSetVerbose condForm = do
  let bool = truthy condForm
  verbose .= bool
  return $ LBool bool

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f xs =
  concatMap (m2l . f) xs
  where m2l (Just x) = [x]
        m2l Nothing  = []

first1 :: (a -> Maybe b) -> [a] -> Maybe b
first1 f xs =
  case (filterMap f xs) of
    []    -> Nothing
    (x:_) -> Just x

lookupSymbol :: LispValue -> [LispFrame] -> LispFrame -> LispValue
lookupSymbol symbol stack' globals =
  case first1 (M.lookup str) stack' of
    Just value -> value
    Nothing    ->
      case (M.lookup str globals) of
        Just value -> value
        Nothing    -> failure
  where str = getStr symbol
        failure = LError $ "Unbound symbol: " ++ (getStr symbol)

evalSymbol :: LispValue -> Lisp LispValue
evalSymbol symbol = do
  rtState <- get
  let stack'  = rtState ^. stack
      globals = rtState ^. globalBindings
  return $ lookupSymbol symbol stack' globals

unLList :: LispValue -> [LispValue]
unLList (LList xs) = xs
unLList _          = error "unLList needs a list"

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

isClosure :: LispValue -> Bool
isClosure (LClosure _) = True
isClosure _            = False

evalLambda :: LispValue -> Lisp LispValue
evalLambda lambdaForm = do
  rtState <- get
  let pos   = getPos lambdaForm
      (fName, params, body) = if isSymbol $ pos 1
                              then ((Just $ pos 1), (unLList $ pos 2), (pos 3))
                              else (Nothing       , (unLList $ pos 1), (pos 2))
      currentStack = rtState ^. stack
  return $ LClosure $ LispClosure fName params currentStack body

checkRestMarker :: LispValue -> Maybe LispValue
checkRestMarker v@(LSymbol ('&':_)) = Just v
checkRestMarker _                   = Nothing

variarityCheck :: [LispValue] -> Maybe LispValue
variarityCheck [] = Nothing
variarityCheck xs = checkRestMarker $ last xs

matchParamsAndValues :: [LispValue] -> [LispValue] -> Either LispValue LispFrame
matchParamsAndValues params values =
  let nParams = length params
  in case variarityCheck params of
    (Just rest) -> if (length values) >= nParams - 1
                   then
                     let params' = init params
                         restValues = drop (nParams - 1) values
                     in Right (M.fromList $ zip (map getStr (rest:params'))
                                                ((LList restValues):values))
                   else Left $ invalidArg "(arity mismatch)" $
                        [LList params, LList values]
    Nothing -> if (length params) == (length values)
               then Right $ M.fromList $ zip (map getStr params) values
               else Left  $ invalidArg "(arity mismatch)" $
                    [LList params, LList values]

        --M.fromList $ zip (map getStr params) argValues

runClosure :: LispClosure -> [LispValue] -> Lisp LispValue
runClosure self@(LispClosure maybeName params stack' body) argValues = do
  case matchParamsAndValues params argValues of
    Right newFrame' -> do
      let newFrame = case maybeName of
                       Just name -> M.insert (getStr name) (LClosure self) newFrame'
                       Nothing   -> newFrame'
      nFrames <- pushFrames (newFrame:stack')
      result  <- eval body
      popFrames nFrames
      return result
    Left anError -> return anError

apply :: [LispValue] -> Lisp LispValue
apply [] = error "apply : given an empty list"
apply form@(form1:args) =
  case form1 of
    LFunction (PrimFn _ f) -> return $ f args
    LClosure closure       -> runClosure closure args
    it@(LError _)          -> return it
    LMacro _ fnOrClosure   -> apply (fnOrClosure:args)
    _                      -> return $ invalidArg "(apply)" form

macroCheck :: LispValue -> Bool
macroCheck (LMacro _ _) = True
macroCheck _            = False

evalListForm :: LispValue -> Lisp LispValue

evalListForm lv@(LList lispValues) =
  case lispValues of
  []                     -> return lv -- empty list: self-evaluating.
  ((LSymbol "lambda"):_) -> evalLambda lv
  (v:vs) -> do
    f <- eval v
    if macroCheck f
    then do
      form1 <- apply $ f:vs
      eval form1
    else do
      args <- sequence (map eval vs)
      apply $ f:args

evalListForm _ = error $ "evalListForm : requires an LList"

cond :: [(Bool, a)] -> a
cond [] = undefined
cond ((True,  x):_ ) = x
cond ((False, _):ls) = cond ls

vPrint :: String -> Lisp ()
vPrint str = (use verbose) >>= (\b -> when b (lift $ putStrLn str))

verbosely :: (Show a) => Lisp String -> (a -> Lisp String) -> Lisp a -> Lisp a
verbosely before after act = do
  before >>= vPrint
  result <- act
  (after result) >>= vPrint
  return result

evalCore :: LispValue -> Lisp LispValue
evalCore lispValue = do
  cond [(selfEvaluating lispValue,   return lispValue),
        (lispValue `headIs` "quote", return $ getPos lispValue 1),
        (lispValue `headIs` "def",
         evalDef (getPos lispValue 1) (getPos lispValue 2)),
        (lispValue `headIs` "if",
         evalIf (getPos lispValue 1) (getPos lispValue 2) (getPos lispValue 3)),
        (lispValue `headIs` "quit",
         error "user quit"),
        (lispValue `headIs` "set-verbose", evalSetVerbose (getPos lispValue 1)),
        (isSymbol lispValue,     evalSymbol lispValue),
        (isListForm lispValue,   evalListForm lispValue),
        (True, error (show lispValue))
       ]

genstr :: Lisp String
genstr = do
  n <- use gensymCounter
  gensymCounter += 1
  return $ "G__" ++ (show n)

eval :: LispValue -> Lisp LispValue
eval lispValue =
  do gs <- genstr
     verbosely (return $ gs ++ "[eval] " ++ (show lispValue))
       (\result -> return $ gs ++ "-----> " ++ (show result))
       (evalCore lispValue)

lispCar :: LispValue
lispCar = LFunction $ PrimFn "car" (\x -> case x of
                        [(LList (hd:_))] -> hd
                        _                -> invalidArg "car" x)

lispCdr :: LispValue
lispCdr = LFunction $ PrimFn "cdr" (\x -> case x of
                        [(LList (_:tl))] -> LList tl
                        _                -> invalidArg "cdr" x)

lispCons :: LispValue
lispCons = LFunction $ PrimFn "cons" (\x -> case x of
                          [x1, (LList xs)] -> LList (x1:xs)
                          _                -> invalidArg "cons" x)

primEqList :: [LispValue] -> [LispValue] -> Bool
primEqList l1 l2 =
  (length l1 == length l2) && (and $ zipWith primEq l1 l2)

primEq :: LispValue -> LispValue -> Bool
primEq v1 v2 =
  case (v1, v2) of
    (LString s1, LString s2) -> s1 == s2
    (LSymbol s1, LSymbol s2) -> s1 == s2
    (LNumber d1, LNumber d2) -> d1 == d2
    (LBool   b1, LBool   b2) -> b1 == b2
    (LList   l1, LList   l2) -> primEqList l1 l2
    _                        -> False

lispEq :: LispValue
lispEq = LFunction $ PrimFn "eq" (\x -> case x of
                        [x1, x2] -> LBool $ primEq x1 x2
                        _        -> invalidArg "eq" x)

primAtom :: LispValue -> Bool
primAtom (LString _) = True
primAtom (LSymbol _) = True
primAtom (LNumber _) = True
primAtom (LBool   _) = True
primAtom (LList  []) = True
primAtom _           = False

lispAtom :: LispValue
lispAtom = LFunction $ PrimFn "atom" (\x -> case x of
                          [x1] -> LBool $ primAtom x1
                          _    -> invalidArg "atom" x)

mkMacro :: LispValue
mkMacro = LFunction $ PrimFn "macro"
                        (\x -> case x of
                            [(LSymbol name), val] -> LMacro name val
                            _                     -> invalidArg "macro" x)

macroAndFn :: LispValue
macroAndFn = LFunction $ PrimFn "and-fn"
                         (\xs -> case xs of
                                 []         -> LBool True
                                 (x:[])     -> x
                                 (x1:x2:[]) -> LList [LSymbol "if",
                                                      x1, x2, LBool False]
                                 (x1:x2:xr) -> LList [LSymbol "if",
                                                      x1,
                                                      LList ((LSymbol "and"):x2:xr),
                                                      LBool False])

macroAnd :: LispValue
macroAnd = LMacro "and" macroAndFn

initGlobal :: LispFrame
initGlobal = M.fromList [("+",  liftToLisp2 (\x y -> (x :: Double) + y) "+"),
                         ("-",  liftToLisp2 (\x y -> (x :: Double) - y) "-"),
                         ("*",  liftToLisp2 (\x y -> (x :: Double) * y) "*"),
                         ("/",  liftToLisp2 (\x y -> (x :: Double) / y) "/"),
                         ("==", liftToLisp2 (\x y -> (x :: Double) == y) "=="),
                         ("/=", liftToLisp2 (\x y -> (x :: Double) /= y) "/="),
                         ("<",  liftToLisp2 (\x y -> (x :: Double) < y)  "<"),
                         ("<=", liftToLisp2 (\x y -> (x :: Double) <= y) "<="),
                         (">",  liftToLisp2 (\x y -> (x :: Double) > y)  ">"),
                         (">=", liftToLisp2 (\x y -> (x :: Double) >= y) ">="),
                         ("and", macroAnd),
                         ("atom", lispAtom),
                         ("car", lispCar),
                         ("cdr", lispCdr),
                         ("cons", lispCons),
                         ("eq", lispEq),
                         ("not", liftToLisp1 not "not"),
                         ("macro", mkMacro),
                         ("pi", LNumber pi)]

initRT :: RuntimeState
initRT = RuntimeState {_globalBindings = initGlobal,
                       _verbose        = False,
                       _stack          = [],
                       _gensymCounter  = 1}

prompt :: String
prompt = "λισπ> "

flush :: IO ()
flush = hFlush stdout

lispPrint :: LispValue -> IO ()
lispPrint = putStrLn . lispShow

repl :: Lisp ()
repl = forever $ do lift $ putStr prompt
                    lift flush
                    inString <- lift getLine
                    let lisp = readString inString
                    result <- eval lisp
                    lift $ lispPrint result

main :: IO ()
main = repl `runLisp` initRT >> return ()
