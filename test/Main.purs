module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Either (Either(Right, Left))
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
children=banana,grape,pineapple
[section2]
bat=grey
[WOWSECTION]
[麻婆豆腐]
"""

type TestIni =
  { section1 ::
       { fruit :: String
       , isRed :: Boolean
       , seeds :: Int
       , children :: Array String
       }
  , section2 ::
       { bat :: String
       }
  , "WOWSECTION" ::
       {}
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
          equal result.section1.children ["banana","grape","pineapple"]
          equal result.section2.bat "grey"
