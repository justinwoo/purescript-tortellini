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
import Type.Prelude (RProxy(..))
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

type Parsellini a = Except UhOhSpaghettios a

parsellIni :: forall rl row
   . RowToList row rl
  => ReadLevel rl () row (StrMap (StrMap String))
  => String
  -> Either UhOhSpaghettios (Record row)
parsellIni s = do
  runExcept $ parsellIni' s

parsellIni' :: forall rl row
   . RowToList row rl
  => ReadLevel rl () row (StrMap (StrMap String))
  => String
  -> Except UhOhSpaghettios (Record row)
parsellIni' s = do
  doc <- except $ lmap (pure <<< ErrorInParsing) $ parseIniDocument s
  builder <- readLevel (RLProxy :: RLProxy rl) doc
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
    readLevel
      :: RLProxy xs
      -> strmap
      -> Except UhOhSpaghettios (Builder { | from } { | to })

instance nilReadLevel :: ReadLevel Nil () () strmap where
  readLevel _ _ = pure id

instance consReadLevelSection ::
  ( IsSymbol name
  , RowCons name ty from' to
  , RowLacks name from'
  , ReadIniField ty
  , ReadLevel tail from from' (StrMap String)
  ) => ReadLevel (Cons name ty tail) from to (StrMap String) where
  readLevel _ sm = do
    levelOperation
      { name: SProxy :: SProxy name
      , from: RProxy :: RProxy from
      , from': RProxy :: RProxy from'
      , to: RProxy :: RProxy to
      , tail: RLProxy :: RLProxy tail
      }
      ErrorAtSectionProperty
      readIniField
      readLevel
      sm

instance consReadLevelDocument ::
  ( IsSymbol name
  , RowCons name (Record inner) from' to
  , RowLacks name from'
  , RowToList inner xs
  , ReadLevel xs () inner (StrMap String)
  , ReadLevel tail from from' (StrMap (StrMap String))
  ) => ReadLevel (Cons name (Record inner) tail) from to (StrMap (StrMap String)) where
  readLevel _ sm = do
    levelOperation
      { name: SProxy :: SProxy name
      , from: RProxy :: RProxy from
      , from': RProxy :: RProxy from'
      , to: RProxy :: RProxy to
      , tail: RLProxy :: RLProxy tail
      }
      ErrorAtDocumentProperty
      (map (Builder.build <@> {}) <<< (readLevel (RLProxy :: RLProxy xs)))
      readLevel
      sm

levelOperation
  :: forall item ty tail from from' to name
   . IsSymbol name
  => RowCons name ty from' to
  => RowLacks name from'
  => { name :: SProxy name
     , from :: RProxy from
     , from' :: RProxy from'
     , to :: RProxy to
     , tail :: RLProxy tail
     }
  -> (String -> UhOhSpaghetto -> UhOhSpaghetto)
  -> (item -> Parsellini ty)
  -> (RLProxy tail -> StrMap item -> Parsellini (Builder { | from } { | from' }))
  -> StrMap item -> Parsellini (Builder { | from } { | to })
levelOperation _ mkError fieldOp restOp sm =
  case SM.lookup name sm of
    Nothing ->
      throwError <<< pure <<< mkError name <<< Error
      $ "Missing property in level"
    Just field -> do
      let withExcept' = withExcept <<< map $ mkError name
      value <- withExcept' $ fieldOp field
      rest <- restOp (RLProxy :: RLProxy tail) sm
      let
        first :: Builder (Record from') (Record to)
        first = Builder.insert nameP value
      pure $ first <<< rest
  where
    nameP = SProxy :: SProxy name
    name = reflectSymbol nameP
