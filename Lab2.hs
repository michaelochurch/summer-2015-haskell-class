-- Lisp design goals
-- Implement the following:

-- S-expression parsing Data.Parsec
-- read, eval, print
-- car, cdr, cons, eq, atom, quote, if
-- +, -, *, /, <, >, >=, ==, /=, and, or as macros

-- lambda -- include recursion

-- Values include: Number, List, Symbol, Bool, String, Nil(?)

-- def, defun, defmacro
-- apply

-- load a file that defines factorial and map and range. use at repl.
-- none of this needs to be tail recursive or fast.

module Main where

import Control.Applicative hiding (many, (<|>))
import Data.Char
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

parseSExp :: Parsec String u SExp
parseSExp = parseList <|> parseString <|> parseAtom

data Lisp = LString String | LSymbol String | LNumber Double

readLisp :: SExp -> Lisp
readLisp = undefined

main :: IO ()
main = return ()
