module Tortellini where

import Prelude

import Control.Monad.Except (Except, except, runExcept, throwError, withExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Int (fromNumber)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), split, toLower)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Foreign.Object (Object)
import Foreign.Object as SM
import Global (readInt)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record.Builder (Builder)
import Record.Builder as Builder
import Text.Parsing.StringParser (ParseError)
import Tortellini.Parser (parseIniDocument)
import Type.Prelude (RLProxy(..))

data UhOhSpaghetto
  = Error String
  | ErrorMissingProperty
  | ErrorAtDocumentProperty String UhOhSpaghetto
  | ErrorAtSectionProperty String UhOhSpaghetto
  | ErrorInParsing ParseError
instance showUhOhSpaghetto :: Show UhOhSpaghetto where
  show (Error s) = s
  show ErrorMissingProperty = "ErrorMissingProperty"
  show (ErrorAtDocumentProperty s e) = "ErrorAtDocumentProperty " <> s <> " (" <> (show e) <> ")"
  show (ErrorAtSectionProperty s e) = "ErrorAtSectionProperty " <> s <> " (" <> (show e) <> ")"
  show (ErrorInParsing s) = "ErrorInParsing (" <> show s <> ")"

type UhOhSpaghettios = NonEmptyList UhOhSpaghetto

type Parsellini a = Except UhOhSpaghettios a

parsellIni :: forall rl row
   . RowToList row rl
  => ReadLevel rl () row (Object (Object String))
  => String
  -> Either UhOhSpaghettios (Record row)
parsellIni s = do
  runExcept $ parsellIni' s

parsellIni' :: forall rl row
   . RowToList row rl
  => ReadLevel rl () row (Object (Object String))
  => String
  -> Except UhOhSpaghettios (Record row)
parsellIni' s = do
  doc <- except $ lmap (pure <<< ErrorInParsing) $ parseIniDocument s
  builder :: Builder {} { | row } <- readLevel (RLProxy :: RLProxy rl) doc
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

class ReadLevel
  (xs :: RowList)
  (from :: # Type) (to :: # Type)
  strmap
  | xs strmap -> from to
  where
    readLevel :: RLProxy xs -> strmap -> Except UhOhSpaghettios (Builder { | from } { | to })

instance nilReadLevel :: ReadLevel Nil () () strmap where
  readLevel _ _ = pure identity

instance consReadLevelSection ::
  ( IsSymbol name
  , Row.Cons name ty from' to
  , Row.Lacks name from'
  , ReadIniField ty
  , ReadLevel tail from from' (Object String)
  ) => ReadLevel (Cons name ty tail) from to (Object String) where
  readLevel _ sm =
    case SM.lookup name sm of
      Nothing ->
        throwError $ pure (ErrorAtSectionProperty name ErrorMissingProperty)
      Just field -> do
        let
          withExcept' = withExcept <<< map $ ErrorAtSectionProperty name
          tailP = RLProxy :: RLProxy tail
          value = withExcept' (readIniField field)
          rest = readLevel tailP sm
          first = Builder.insert nameP <$> value
        compose <$> first <*> rest
    where
      nameP = SProxy :: SProxy name
      name = reflectSymbol nameP

else instance consReadLevelDocument ::
  ( IsSymbol name
  , Row.Cons name (Record inner) from' to
  , Row.Lacks name from'
  , RowToList inner xs
  , ReadLevel xs () inner a
  , ReadLevel tail from from' (Object a)
  ) => ReadLevel (Cons name (Record inner) tail) from to (Object a) where
  readLevel _ sm =
    case SM.lookup name sm of
      Nothing ->
        throwError $ pure (ErrorAtDocumentProperty name ErrorMissingProperty)
      Just section -> do
        let
          withExcept' = withExcept <<< map $ ErrorAtSectionProperty name
          tailP = RLProxy :: RLProxy tail
          xsP = RLProxy :: RLProxy xs
          builder = withExcept' (readLevel xsP section)
          rest = readLevel tailP sm
          value = Builder.build <@> {} <$> builder
          first = Builder.insert nameP <$> value
        compose <$> first <*> rest
    where
      nameP = SProxy :: SProxy name
      name = reflectSymbol nameP
