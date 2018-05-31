module Test.Main where

import Prelude

import Data.Either (Either(Right, Left), either)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (throw)
import Test.Assert (assert)
import Text.Parsing.StringParser (runParser)
import Tortellini (parsellIni)
import Tortellini.Parser (document, field, sectionName)

testDoc :: String
testDoc = """
[section1]
fruit=apple
isRed=true
seeds=4
[MOOMINJUMALA]
children=banana,grape,pineapple
[麻婆豆腐]
"""

type TestIni =
  { section1 ::
       { fruit :: String
       , isRed :: Boolean
       , seeds :: Int
       }
  , "MOOMINJUMALA" ::
       { children :: Array String
       }
  , "麻婆豆腐" ::
       {}
  }

equal :: forall a. Eq a => a -> a -> Effect Unit
equal a b =
  assert (a == b)

assert' :: String -> Boolean -> Effect Unit
assert' s cond =
  if cond
     then pure unit
     else throw s

main :: Effect Unit
main = do
  -- "parser"
  -- "sectionName"
  case runParser sectionName "[name]" of
    Left e -> throw $ show e
    Right a -> assert' "should be 'name'" $ a == "name"

  -- field
  case runParser field "apple=banana" of
    Left e -> throw $ show e
    Right a -> assert' "should be 'name'" $ a == (Tuple "apple" "banana")

  -- document
  case runParser document testDoc of
    Left e -> throw $ show e
    Right _ -> pure unit

  -- "parsellIni"
  -- "works"
  case parsellIni testDoc of
    Left e -> throw $ show e
    Right (result :: TestIni) -> do
      equal result.section1.fruit "apple"
      equal result.section1.isRed true
      equal result.section1.seeds 4
      equal result."MOOMINJUMALA".children ["banana","grape","pineapple"]

  -- "works2"
  case parsellIni testDoc of
    Left e -> throw $ show e
    Right (result :: {section1 :: {fruit :: String}}) -> do
      equal result.section1.fruit "apple"

  -- "works3"
  let
    equal' :: {section1 :: {fruit :: String}} -> Effect Unit
    equal' r = equal r.section1.fruit "apple"
  either (throw <<< show) equal' $ parsellIni testDoc
