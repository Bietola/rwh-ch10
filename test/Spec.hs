import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Test.HUnit

import Lib

tests =
  TestList
    [ "matchHeaderVersion" ~:
      TestList
        [ "Trivial scenario" ~:
          doParse
            (matchHeaderVersion (L8.pack "simple"))
            (L8.pack "simple 1 2 3") ~?=
          Just ((), L8.pack " 1 2 3")
        , "Composed with skipSpace parser" ~:
          let composedParser =
                matchHeaderVersion (L8.pack "simple") *>= (\_ -> skipSpaces)
           in (doParse composedParser) (L8.pack "simple   1 2 3") ~?=
              Just ((), L8.pack "1 2 3")
        ]
    , "parseP5" ~:
      TestList
        [ "Trivial scenario" ~:
          (doParse parseP5)
            (L8.pack
               "P5\n3 2\n200\n123456") ~?=
          Just
            ( Greymap
                3
                2
                200
                (L8.pack
                   "123456")
            , L8.empty)
        , "Trivial scenario, incomplete input" ~:
          (doParse parseP5) (L8.pack "P5\n10 100\n") ~?=
          Nothing
        ]
    , "skipSpaces" ~:
      TestList
        [ "Two space separated numbers" ~:
          let pairParser =
                getNum *>= \num1 ->
                  skipSpaces *>> getNum *>= \num2 -> idParser (num1, num2)
           in doParse pairParser (L8.pack "1 2") ~?= Just ((1, 2), L.empty)
        , "Two newline separated numbers" ~:
          let pairParser =
                getNum *>= \num1 ->
                  skipSpaces *>> getNum *>= \num2 -> idParser (num1, num2)
           in doParse pairParser (L8.pack "1\n2") ~?= Just ((1, 2), L.empty)
        , "Newlines AND spaces" ~:
          let pairParser =
                getNum *>= \num1 ->
                  skipSpaces *>> getNum *>= \num2 ->
                    skipSpaces *>> getNum *>= \num3 ->
                      idParser (num1, num2, num3)
           in doParse pairParser (L8.pack "1 2\n3") ~?=
              Just ((1, 2, 3), L.empty)
        ]
    , "getBytes" ~:
      TestList
        [ "Get 1 byte" ~: doParse (getBytes 1) (L8.pack "NRABS") ~?=
          Just (L8.pack "N", L8.pack "RABS")
        , "Get 5 bytes" ~: doParse (getBytes 5) (L8.pack "RSNRSNRABS") ~?=
          Just (L8.pack "RSNRS", L8.pack "NRABS")
        , "Newlines AND spaces" ~:
          let pairParser =
                getNum *>= \num1 ->
                  skipSpaces *>> getNum *>= \num2 ->
                    skipSpaces *>> getNum *>= \num3 ->
                      idParser (num1, num2, num3)
           in doParse pairParser (L8.pack "1 2\n3") ~?=
              Just ((1, 2, 3), L.empty)
        ]
    ]

main :: IO Counts
main = runTestTT tests
