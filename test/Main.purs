module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Either (Either(Right, Left), either)
import Data.Tuple (Tuple(..))
import Test.Unit (failure, success, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)
import Text.Parsing.StringParser (runParser)
import Tortellini (parseIni)
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

main :: Eff _ Unit
main = runTest do
  suite "parser" do
    test "sectionName" do
      case runParser sectionName "[name]" of
        Left e -> failure $ show e
        Right a -> assert "should be 'name'" $ a == "name"
    test "field" do
      case runParser field "apple=banana" of
        Left e -> failure $ show e
        Right a -> assert "should be 'name'" $ a == (Tuple "apple" "banana")
    test "document" do
      case runParser document testDoc of
        Left e -> failure $ show e
        Right _ -> success
  suite "parseIni" do
    test "works" do
      case parseIni testDoc of
        Left e -> failure $ show e
        Right (result :: TestIni) -> do
          equal result.section1.fruit "apple"
          equal result.section1.isRed true
          equal result.section1.seeds 4
          equal result."MOOMINJUMALA".children ["banana","grape","pineapple"]
    test "works2" do
      case parseIni testDoc of
        Left e -> failure $ show e
        Right (result :: {section1 :: {fruit :: String}}) -> do
          equal result.section1.fruit "apple"
    test "works3" do
      let
        equal' :: {section1 :: {fruit :: String}} -> _
        equal' r = equal r.section1.fruit "apple"
      either (failure <<< show) equal' $ parseIni testDoc