module Tortellini.Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.Foldable (class Foldable)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.Combinators (lookAhead, many1, manyTill)
import Text.Parsing.StringParser.String (char, eof, regex, satisfy)

type IniDocument = Object (Object String)
type Section = Tuple String (Object String)
type Field = Tuple String String

parseIniDocument :: String -> Either ParseError IniDocument
parseIniDocument s = runParser document s

charListToString :: forall f. Foldable f => f Char -> String
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

sectionName :: Parser String
sectionName = lexeme do
  _ <- char '['
  s <- regex "[^\\]]*"
  _ <- char ']'
  pure s

field :: Parser Field
field = lexeme do
  key <- regex "[^=]*"
  _ <- char '='
  value <- regex ".*"
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
