# PureScript-Tortellini

[![Build Status](https://travis-ci.org/justinwoo/purescript-tortellini.svg?branch=master)](https://travis-ci.org/justinwoo/purescript-tortellini)

A library for writing and reading from ini files.

*Disclaimer:* this library does not aim to provide flexible ini support. Only what's easy.

## Assumptions

The main way I work with ini files is to declare that the top level must be a record, with each section header being a label and the section body being a record type at that field.

Each section is then a record of the values, where `key=value` with no spaces, with the following representations:

* Boolean: true or false, unquoted
* Int: 1 as the number not wrapped in anything
* String: string, unquoted
* Array: members represented as their types, with a comma delimiter

## Tl;dr

```hs
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
  
-- ...
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
```

## Extra notes

Things someone might add if they really want it:

* Nullary Sum to String generic to work with enumerable values
    * Do you really need this though?...
* Escaping characters
* Multiline escaping
* Whatever other "nice to have" features

Things that I won't support unless someone has a good idea:

* Records nested in sections. Who wants this anyway?
* Nested arrays. What even?
