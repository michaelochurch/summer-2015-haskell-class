-- Lisp design goals
-- Implement the following:

-- S-expression parsing Data.Parsec
-- read, eval, print
-- car, cdr, cons, eq, atom, quote, if
-- +, -, *, /, <, >, >=, ==, /=, and, or as macros
-- list, of course.
-- lambda -- include recursion

-- Values include: Number, List, Symbol, Bool, String, Nil(?)

-- def, defun, defmacro
-- apply

-- load a file that defines factorial and map and range. use at repl.
-- Lambda calculus / Church numeral demonstration.
-- none of this needs to be tail recursive or fast.

module Main where

import Control.Applicative hiding (many, (<|>))
import Control.Monad.State
import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Token (makeTokenParser, stringLiteral)


data SExp = SAtom String | SList [SExp] deriving (Eq, Show)

parseAtom :: Parsec String u SExp
parseAtom = SAtom <$> many (satisfy (\c -> c `notElem` "()\"" && (not . isSpace) c))

-- I really don't feel like writing my own string literal parser.
stringLit :: Parsec String u String
stringLit = stringLiteral $ makeTokenParser haskellDef

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
                 LFunction (String, [LispValue] -> LispValue) |
                 LList [LispValue] | LBool Bool

instance Show LispValue where
  show (LString str) = show str
  show (LSymbol sym) = sym
  show (LNumber d) = show d
  show (LFunction (name, _)) = "<function called " ++ name ++ ">"
  show (LList lst) = "(" ++ intercalate " " (map show lst) ++ ")"
  show (LBool True) = "#t"
  show (LBool False) = "#f"

readSAtom :: String -> LispValue
readSAtom ""     = LString ""
readSAtom s@(c:cs) =
  case c of
    '\"' -> LString s
    '#'  -> case s of
              "#t" -> LBool True
              "#f" -> LBool False
              _    -> LSymbol s
    d | isDigit d  -> LNumber (read s)
    _              -> LSymbol s

readSExp :: SExp -> LispValue
readSExp (SAtom s) = readSAtom s
readSExp (SList l) = LList $ map readSExp l

readString :: String -> LispValue
readString s =
  case parse parseSExp "(some lisp, I hope)" s of
    Left _     -> error "parse error"
    Right sExp -> readSExp sExp

lispArith2 :: (Double -> Double -> Double) -> String -> LispValue
lispArith2 f name = LFunction (name, f2)
  where f2 xs =
          case xs of
            [(LNumber x1), (LNumber x2)] -> LNumber $ f x1 x2
            _        -> error $ name ++ (intercalate " " $ map show xs)

atom :: LispValue -> Bool
atom (LList _) = False
atom _         = True

selfEvaluating :: LispValue -> Bool
selfEvaluating (LList _) = False
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
    value <- eval valForm
    modify $ M.insert (getStr $ symbol) value
    return $ LBool True

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
        (True, error (show lispValue))
       ]
  -- if atom lispValue
  -- then return lispValue
  -- else if isLambdaForm lispValue
  --      then error "lambdas not implemented yet"
  --      else if headIs lispValue "quote" then

defaultEnv :: LispEnv
defaultEnv = M.fromList [("+", lispArith2 (+) "+")]

-- 1. (quote -- don't eval)
-- 2. (if -- special rules)
-- 3. (lambda -- special rules)
-- 4. (all else-- as function)

main :: IO ()
main = return ()
