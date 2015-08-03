module Lisp.Parser where

import Control.Applicative
import Data.Char (isSpace)
import Text.Parsec hiding (many, (<|>))
import Text.Parsec.String (parseFromFile)

data SExp = SAtom String | SList [SExp] deriving (Eq, Show)

parseAtom :: Parsec String u SExp
parseAtom = SAtom <$> some (satisfy (\c -> c `notElem` "()\";" && (not . isSpace) c))

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
  where body = sepBy parseSExp (some space)

quoteAtom :: SExp
quoteAtom = SAtom "quote"

parseQuoted :: Parsec String u SExp
parseQuoted = (\x -> SList [quoteAtom, x]) <$> (char '\'' *> parseSExp)

parseSExp :: Parsec String u SExp
parseSExp = parseQuoted <|> parseList <|> parseString <|> parseAtom

ignore :: Parsec String u a -> Parsec String u ()
ignore parser = parser *> return ()

parseComment :: Parsec String u ()
parseComment = char ';' *> (ignore $ manyTill anyChar ((ignore $ char '\n') <|> eof))

parseInert :: Parsec String u ()
parseInert = ignore $ many (ignore space <|> parseComment)

parseSExps :: Parsec String u [SExp]
parseSExps = parseInert *> endBy parseSExp parseInert

loadSExps :: String -> IO (Either String [SExp])
loadSExps filename = do
  result <- parseFromFile parseSExps filename
  case result of
    Right sexps   -> return $ Right sexps
    Left  anError -> return $ Left (show anError)
