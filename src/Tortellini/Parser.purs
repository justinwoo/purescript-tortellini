module Tortellini.Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.List (List)
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.Combinators (lookAhead, many1, many1Till, manyTill)
import Text.Parsing.StringParser.String (anyChar, char, eof, oneOf, satisfy)

type IniDocument = Object (Object String)
type Section = Tuple String (Object String)
type Field = Tuple String String

parseIniDocument :: String -> Either ParseError IniDocument
parseIniDocument s = runParser document s

charListToString :: List Char -> String
charListToString = fromCharArray <<< fromFoldable

skipSpace :: Parser Unit
skipSpace = fix \_ ->
  (many1 ws *> skipSpace) <|> pure unit
  where
    ws = satisfy \c ->
      c == '\n' ||
      c == '\r' ||
      c == '\t' ||
      c == ' '

lexeme :: forall a. Parser a -> Parser a
lexeme p = p <* skipSpace

many1TillString :: forall a. Parser Char -> Parser a -> Parser String
many1TillString p end = charListToString <$> many1Till p end

sectionName :: Parser String
sectionName = lexeme do
  _ <- char '['
  name <- many1Till anyChar (char ']')
  pure $ charListToString name

field :: Parser Field
field = lexeme do
  key <- charListToString <$> many1Till anyChar (char '=')
  value <- charListToString <$> many1Till anyChar
    (oneOf ['\n', '\r'] *> pure unit <|> eof)
  pure $ Tuple key value

section :: Parser Section
section = lexeme do
  name <- sectionName
  body <- Object.fromFoldable <$> manyTill field
    (lookAhead (char '[') *> pure unit <|> eof)
  pure $ Tuple name body

document :: Parser IniDocument
document = lexeme do
  skipSpace
  Object.fromFoldable <$> many1 section
