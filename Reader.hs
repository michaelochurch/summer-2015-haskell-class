module Reader where

import Data.Char (isDigit)
import Text.Parsec

import Parser
import Types

dequoteStringLiteral :: String -> String
dequoteStringLiteral s =
  if length s >= 2 && (head s) == '"' && (last s) == '"'
  then tail . init $ s
  else error "dequoteStringLiteral : requires a string literal"

readSAtom :: String -> LispValue
readSAtom "" = LVString ""
readSAtom s@(c:_) =
  case c of
    '\"' -> LVString $ dequoteStringLiteral s
    '#' -> case s of
      "#t" -> LVBool True
      "#f" -> LVBool False
      _    -> LVSymbol s
    '-' -> if s == "-" || (not . isDigit) (s !! 1)
           then LVSymbol s
           else LVNumber (read s)
    d | isDigit d -> LVNumber (read s)
    _ -> LVSymbol s

readSExp :: SExp -> LispValue
readSExp (SAtom s) = readSAtom s
readSExp (SList l) = LVList $ map readSExp l

readString :: String -> Either ParseError LispValue
readString s = fmap readSExp $ parse parseSExp "" s
