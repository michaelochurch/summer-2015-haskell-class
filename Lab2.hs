-- Lisp design goals
-- Implement the following:

-- S-expression parsing Data.Parsec
-- read, eval, print
-- car, cdr, cons, eq, atom, quote, if
--  and, or as macros
-- list, of course.
-- lambda -- include recursion

-- def, defun, defmacro
-- apply

-- load a file that defines factorial and map and range. use at repl.
-- Lambda calculus / Church numeral demonstration.
-- none of this needs to be tail recursive or fast.

module Main where

import Control.Applicative hiding (many, (<|>))
import Control.Monad.State
import Data.Char (isDigit, isSpace)
import Data.List (foldl', intercalate)
import qualified Data.Map as M
import System.IO (hFlush, stdout)
import Text.Parsec
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Token (makeTokenParser, stringLiteral)


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

data LispValue = LString String | LSymbol String | LNumber Double |
                 LFunction String ([LispValue] -> LispValue) |
                 LLambda (Maybe LispValue) [LispValue] LispEnv LispValue |
                 LList [LispValue] | LBool Bool | LError String

instance Show LispValue where
  show (LString str) = show str
  show (LSymbol sym) = sym
  show (LNumber d) = show d
  show (LFunction name _) = "< prim. function called " ++ name ++ " >"
  show (LList lst) = "(" ++ intercalate " " (map show lst) ++ ")"
  show (LBool True) = "#t"
  show (LBool False) = "#f"
  show (LLambda maybeName symbols _env body) =
    "< " ++ name ++ "λ " ++ (show . LList $ symbols) ++ " . " ++ (show body) ++ " >"
       where name = case maybeName of
                      Just sym -> (show sym) ++ " = "
                      Nothing -> ""
  show (LError string) = "< ERROR: " ++ string ++ " >"

dequoteStringLiteral :: String -> String
dequoteStringLiteral s =
  if length s >= 2 && (head s) == '"' && (last s) == '"'
  then tail . init $ s
  else error "dequoteStringLiteral : that wasn't a string literal"


readSAtom :: String -> LispValue
readSAtom ""     = LString ""
readSAtom s@(c:cs) =
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
  case parse parseSExp "(some lisp, I hope)" s of
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
  LError $ "Invalid Argument Error: (" ++ name ++ " " ++ (intercalate " " $ map show vals) ++ ")"

liftToLisp1 :: (Liftable a, Liftable b) => (a -> b) -> String -> LispValue
liftToLisp1 f name = LFunction name f1
  where f1 xs@[x1] =
          case fromLisp x1 of
            Just v1 -> toLisp $ f v1
            _       -> invalidArg name xs
        f1 xs = invalidArg name xs

liftToLisp2 :: (Liftable a, Liftable b, Liftable c) => (a -> b -> c) -> String -> LispValue
liftToLisp2 f name = LFunction name f2
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

isLambdaForm :: LispValue -> Bool
isLambdaForm (LList ((LSymbol "lambda"):_)) = True
isLambdaForm _                              = False

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

type LispEnv = M.Map String LispValue

type Lisp a = StateT LispEnv IO a

runLisp :: Lisp a -> LispEnv -> IO (a, LispEnv)
runLisp lispAction env = lispAction `runStateT` env

truthy :: LispValue -> Bool
truthy (LBool False) = False
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
    cond <- eval condForm
    if truthy cond
    then eval thenForm
    else eval elseForm

evalDef :: LispValue -> LispValue -> Lisp LispValue
evalDef symbol valForm =
  do
    modify $ M.delete (getStr symbol) -- avoid what is being def'd capturing a stale version of itself.
    value <- eval valForm
    modify $ M.insert (getStr symbol) value
    return $ LBool True

evalSymbol :: LispValue -> Lisp LispValue
evalSymbol symbol = do
  env <- get
  case M.lookup (getStr symbol) env of
   Just value -> return value
   Nothing    -> return $ LError $ "Unbound symbol: " ++ (getStr symbol)

unLList :: LispValue -> [LispValue]
unLList (LList xs) = xs
unLList _          = error "unLList needs a list"

-- HAS TO CAPTURE ENVIRONMENT OR THE WORLD IS FUCKED
evalLambda :: LispValue -> Lisp LispValue
evalLambda lambdaList = do
  env <- get
  let pos = getPos lambdaList
  if (isSymbol $ pos 1)
    then return $ LLambda (Just $ pos 1) (unLList $ pos 2) env (pos 3)
    else return $ LLambda Nothing        (unLList $ pos 1) env (pos 2)

apply :: [LispValue] -> Lisp LispValue
apply ((LFunction _ f):args) = return $ f args
apply (lform@(LLambda maybeFName argNames env body):args) =
  applyLambda lform args
apply (e@(LError _):_) = return e
apply _ = error "apply : requires a function"

envPlus :: LispEnv -> [(LispValue, LispValue)] -> LispEnv
envPlus env list =
  foldl' (\e (name, val) -> M.insert (getStr name) val e) env list


applyLambda :: LispValue -> [LispValue] -> Lisp LispValue
applyLambda lambdaObj args = do
  let (LLambda maybeFName argNames env body) = lambdaObj
  argValues <- sequence (map eval args)
  env0      <- get
--  let env1 = env0 `envPlus` zip argNames args
  let env1 = (env `envPlus` zip argNames args) `M.union` env0
  put env1
  result    <- eval body
  put env0
  return result

evalListForm :: LispValue -> Lisp LispValue
evalListForm lv@(LList lispValues) =
  case lispValues of
  []                     -> return lv -- empty list: self-evaluating.
  ((LSymbol "lambda"):_) -> evalLambda lv
  valueList -> do
    args <- sequence (map eval valueList)
    apply args
evalListFrom _ = error $ "evalListForm : requires an LList"

cond :: [(Bool, a)] -> a
cond [] = undefined
cond ((True,  x):_ ) = x
cond ((False, _):ls) = cond ls

eval :: LispValue -> Lisp LispValue
eval lispValue =
  cond [(selfEvaluating lispValue,   return lispValue),
        (lispValue `headIs` "quote", return $ getPos lispValue 1),
        (lispValue `headIs` "def",
         evalDef (getPos lispValue 1) (getPos lispValue 2)),
        (lispValue `headIs` "if",
         evalIf (getPos lispValue 1) (getPos lispValue 2) (getPos lispValue 3)),
        (lispValue `headIs` "quit",
         error "user quit"),
        (isSymbol lispValue,   evalSymbol lispValue),
        (isListForm lispValue, evalListForm lispValue),
        (True, error (show lispValue))
       ]

car :: LispValue
car = LFunction "car" (\x -> case x of
                        [(LList (hd:_))] -> hd
                        _                -> invalidArg "car" x)

cdr :: LispValue
cdr = LFunction "cdr" (\x -> case x of
                        [(LList (_:tl))] -> LList tl
                        _                -> invalidArg "cdr" x)

cons :: LispValue
cons = LFunction "cons" (\x -> case x of
                          [x1, (LList xs)] -> LList (x1:xs)
                          -- this is non-conventional but I don't feel like including
                          -- dotted pairs
                          _                -> invalidArg "cons" x)

-- eq :: LispValue
-- eq = LFunction "

list :: LispValue
list = undefined

defaultEnv :: LispEnv
defaultEnv = M.fromList [("+",  liftToLisp2 (\x y -> (x :: Double) + y) "+"),
                         ("-",  liftToLisp2 (\x y -> (x :: Double) - y) "-"),
                         ("*",  liftToLisp2 (\x y -> (x :: Double) * y) "*"),
                         ("/",  liftToLisp2 (\x y -> (x :: Double) / y) "/"),
                         ("==", liftToLisp2 (\x y -> (x :: Double) == y) "=="),
                         ("/=", liftToLisp2 (\x y -> (x :: Double) /= y) "/="),
                         ("<",  liftToLisp2 (\x y -> (x :: Double) < y)  "<"),
                         ("<=", liftToLisp2 (\x y -> (x :: Double) <= y) "<="),
                         (">",  liftToLisp2 (\x y -> (x :: Double) > y)  ">"),
                         (">=", liftToLisp2 (\x y -> (x :: Double) >= y) ">="),
                         ("car", car),
                         ("cdr", cdr),
                         ("cons", cons),
                         ("not", liftToLisp1 not "not")]

prompt :: String
prompt = "λισπ> "

flush :: IO ()
flush = hFlush stdout

-- BUG : parse of "()" is incorrect. It has an empty string for the car, I think.

repl :: Lisp ()
repl = forever $ do lift $ putStr prompt
                    lift flush
                    inString <- lift getLine
                    let lisp = readString inString
                    result <- eval lisp
                    lift $ print result

main :: IO ()
main = repl `runLisp` defaultEnv >> return ()
