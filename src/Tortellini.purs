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
import Type.Prelude (RProxy(..))
import Type.Row (RLProxy(..))

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
    readLevel
      :: RLProxy xs
      -> strmap
      -> Except UhOhSpaghettios (Builder { | from } { | to })

instance nilReadLevel :: ReadLevel Nil () () strmap where
  readLevel _ _ = pure identity

instance consReadLevelSection ::
  ( IsSymbol name
  , Row.Cons name ty from' to
  , Row.Lacks name from'
  , ReadIniField ty
  , ReadLevel tail from from' (Object String)
  ) => ReadLevel (Cons name ty tail) from to (Object String) where
  readLevel _ =
    levelOperation
      { name: SProxy :: SProxy name
      , from: RProxy :: RProxy from
      , from': RProxy :: RProxy from'
      , to: RProxy :: RProxy to
      , tail: RLProxy :: RLProxy tail
      }
      ErrorAtSectionProperty
      readIniField

instance consReadLevelDocument ::
  ( IsSymbol name
  , Row.Cons name (Record inner) from' to
  , Row.Lacks name from'
  , RowToList inner xs
  , ReadLevel xs () inner (Object String)
  , ReadLevel tail from from' (Object (Object String))
  ) => ReadLevel (Cons name (Record inner) tail) from to (Object (Object String)) where
  readLevel _ =
    levelOperation
      { name: SProxy :: SProxy name
      , from: RProxy :: RProxy from
      , from': RProxy :: RProxy from'
      , to: RProxy :: RProxy to
      , tail: RLProxy :: RLProxy tail
      }
      ErrorAtDocumentProperty
      (map (Builder.build <@> {}) <<< (readLevel (RLProxy :: RLProxy xs)))

levelOperation
  :: forall item ty tail from from' to name
   . IsSymbol name
  => Row.Cons name ty from' to
  => Row.Lacks name from'
  => ReadLevel tail from from' (Object item)
  => { name :: SProxy name
     , from :: RProxy from
     , from' :: RProxy from'
     , to :: RProxy to
     , tail :: RLProxy tail
     }
  -> (String -> UhOhSpaghetto -> UhOhSpaghetto)
  -> (item -> Parsellini ty)
  -> Object item
  -> Parsellini (Builder { | from } { | to })
levelOperation _ mkError fieldOp sm =
  case SM.lookup name sm of
    Nothing ->
      throwError <<< pure <<< mkError name <<< Error
      $ "Missing property in level"
    Just field -> do
      let withExcept' = withExcept <<< map $ mkError name
      value <- withExcept' $ fieldOp field
      rest <- readLevel (RLProxy :: RLProxy tail) sm
      let
        first :: Builder (Record from') (Record to)
        first = Builder.insert nameP value
      pure $ first <<< rest
  where
    nameP = SProxy :: SProxy name
    name = reflectSymbol nameP
