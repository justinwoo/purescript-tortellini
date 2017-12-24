module Tortellini where

import Prelude

import Control.Monad.Except (Except, except, runExcept, throwError, withExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Int (fromNumber)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), maybe)
import Data.Record.Builder (Builder)
import Data.Record.Builder as Builder
import Data.StrMap (StrMap)
import Data.StrMap as SM
import Data.String (Pattern(..), split, toLower)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Global (readInt)
import Text.Parsing.StringParser (ParseError)
import Tortellini.Parser (parseIniDocument)
import Type.Row (class RowLacks, class RowToList, Cons, Nil, RLProxy(..), kind RowList)

data UhOhSpaghetto
  = Error String
  | ErrorAtDocumentProperty String UhOhSpaghetto
  | ErrorAtSectionProperty String UhOhSpaghetto
  | ErrorInParsing ParseError
instance showUhOhSpaghetto :: Show UhOhSpaghetto where
  show (Error s) = s
  show (ErrorAtDocumentProperty s e) = "ErrorAtDocumentProperty " <> s <> " (" <> (show e) <> ")"
  show (ErrorAtSectionProperty s e) = "ErrorAtSectionProperty " <> s <> " (" <> (show e) <> ")"
  show (ErrorInParsing s) = "ErrorInParsing (" <> show s <> ")"

type UhOhSpaghettios = NonEmptyList UhOhSpaghetto

parseIni :: forall rl row
   . RowToList row rl
  => ReadDocumentSections rl () row
  => String
  -> Either UhOhSpaghettios (Record row)
parseIni s = do
  doc <- lmap (pure <<< ErrorInParsing) $ parseIniDocument s
  builder <- runExcept $ readDocumentSections (RLProxy :: RLProxy rl) doc
  pure $ Builder.build builder {}

parseIni' :: forall rl row
   . RowToList row rl
  => ReadDocumentSections rl () row
  => String
  -> Except UhOhSpaghettios (Record row)
parseIni' s = do
  doc <- except $ lmap (pure <<< ErrorInParsing) $ parseIniDocument s
  builder <- readDocumentSections (RLProxy :: RLProxy rl) doc
  pure $ Builder.build builder {}

class ReadIniField a where
  readIniField :: String -> Except UhOhSpaghettios a

instance stringReadIniField :: ReadIniField String where
  readIniField s = pure s

instance intReadIniField :: ReadIniField Int where
  readIniField s = maybe
    (throwError <<< pure <<< Error $ "Expected Int, got " <> s)
    pure
    $ fromNumber $ readInt 10 s

instance booleanReadIniField :: ReadIniField Boolean where
  readIniField s = case toLower s of
    "true" -> pure true
    "false" -> pure false
    _ -> throwError <<< pure <<< Error $ "Expected true/false, got " <> s

instance arrayReadIniField ::
  ( ReadIniField a
  ) => ReadIniField (Array a) where
  readIniField s = traverse readIniField
    $ split (Pattern ",") s

class ReadDocumentSections (xs :: RowList) (from :: # Type) (to :: # Type)
  | xs -> from to where
  readDocumentSections ::
       RLProxy xs
    -> StrMap (StrMap String)
    -> Except UhOhSpaghettios (Builder (Record from) (Record to))

instance nilReadDocumentSections ::
  ReadDocumentSections Nil () () where
  readDocumentSections _ _ = pure id

instance consReadDocumentSections ::
  ( IsSymbol name
  , RowToList inner xs
  , ReadSection xs () inner
  , RowCons name (Record inner) from' to
  , RowLacks name from'
  , ReadDocumentSections tail from from'
  ) => ReadDocumentSections (Cons name (Record inner) tail) from to where
  readDocumentSections _ sm = do
    case SM.lookup name sm of
      Nothing ->
        throwError <<< pure <<< ErrorAtDocumentProperty name <<< Error
        $ "Missing section in document"
      Just section -> do
        builder <- withExcept' $ readSection (RLProxy :: RLProxy xs) section
        let value = Builder.build builder {}
        rest <- readDocumentSections (RLProxy :: RLProxy tail) sm
        let
          first :: Builder (Record from') (Record to)
          first = Builder.insert nameP value
        pure $ first <<< rest
    where
      nameP = SProxy :: SProxy name
      name = reflectSymbol nameP
      withExcept' = withExcept <<< map $ ErrorAtDocumentProperty name

class ReadSection (xs :: RowList) (from :: # Type) (to :: # Type)
  | xs -> from to where
  readSection ::
       RLProxy xs
    -> StrMap String
    -> Except UhOhSpaghettios (Builder (Record from) (Record to))

instance nilReadSection ::
  ReadSection Nil () () where
  readSection _ _ = pure id

instance consReadSection ::
  ( IsSymbol name
  , ReadIniField ty
  , ReadSection tail from from'
  , RowCons name ty from' to
  , RowLacks name from'
  ) => ReadSection (Cons name ty tail) from to where
  readSection _ sm = do
    case SM.lookup name sm of
      Nothing ->
        throwError <<< pure <<< ErrorAtSectionProperty name <<< Error
        $ "Missing field in section"
      Just field -> do
        value <- withExcept' $ readIniField field
        rest <- readSection (RLProxy :: RLProxy tail) sm
        let
          first :: Builder (Record from') (Record to)
          first = Builder.insert nameP value
        pure $ first <<< rest
    where
      nameP = SProxy :: SProxy name
      name = reflectSymbol nameP
      withExcept' = withExcept <<< map $ ErrorAtSectionProperty name
