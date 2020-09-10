module Lib where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as L (w2c)
import qualified Data.ByteString.Lazy.Char8 as L8

import Data.Char (isDigit, isSpace)
import Data.Int
import Data.Word

-----------------------
-- Greymap datatypes --
-----------------------
data Greymap =
  Greymap
    { greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: L.ByteString
    }
  deriving (Eq)

instance Show Greymap where
  show (Greymap w h m b)
    -- "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m ++ " [...]"
   = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m ++ ": " ++ show b

--------------------------
-- Core Parsing Library --
--------------------------
data ParseState =
  ParseState
    { contents :: L.ByteString
    , offset :: Int64
    }
  deriving (Show)

type ParseResult a = Either String (a, ParseState)

newtype Parser a =
  Parser
    { doParse :: ParseState -> ParseResult a
    }

instance Functor Parser where
  fmap f parser = parser *>= \result -> idParser $ f result

literal :: L.ByteString -> Parser ()
literal = undefined

-- parseNum >>= (\num ->
--   skipSpaces >>= (\() ->
--     parseNum >>= (\num2 -> 
--       return (num, num2))))
(*>=) :: Parser a -> (a -> Parser b) -> Parser b
parser *>= parserGen =
  Parser $ \initState ->
    case (doParse parser initState) of
      Left err -> Left err
      Right (output, newState) -> doParse (parserGen output) newState

(*>>) :: Parser a -> Parser b -> Parser b
parser *>> otherParser = parser *>= \(_) -> otherParser

-- TODO: Try to remove $
idParser :: a -> Parser a
idParser val = Parser $ \state -> Right (val, state)

bail :: String -> Parser a
bail err = Parser $ \_ -> Left err

-- (repeat 10 parseByte) *>= \bytes ->
--   getState *>= \state ->
--     putState ParseState { offset = (offset state) + 10 } *>>
--       skipSpaces *>>
getState :: Parser ParseState
getState = Parser $ \state -> Right (state, state)

putState :: ParseState -> Parser ()
putState newState = Parser $ \oldState -> Right ((), newState)

parseChar :: Parser Char
parseChar = fmap L.w2c parseByte

parseByte :: Parser Word8
parseByte =
  getState *>= \initState ->
    case L.uncons (contents initState) of
      Nothing -> bail "Not enough bytes."
      Just (byte, rest) ->
        putState ParseState {contents = rest, offset = offset initState + 1} *>>
        idParser byte

parseIf :: (a -> Bool) -> Parser a -> Parser a
parseIf parser = undefined

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = undefined

parseWhile :: (a -> Bool) -> Parser a -> Parser [a]
parseWhile pred parser = zeroOrMore (parseIf pred parser)

-----------------------------
-- Greymap Parsing Library --
-----------------------------
matchHeaderVersion :: L.ByteString -> Parser ()
matchHeaderVersion = literal

getNum :: Parser Int
getNum =
  read <$> parseWhile isDigit parseChar

parseBytes :: Int -> Parser L.ByteString
parseBytes amount = fmap lst2bstr (repeat amount parseByte)

skipSpaces :: Parser ()
skipSpaces = fmap (\_ -> ()) (parseWhile isSpace)

parseP5 :: Parser Greymap
parseP5 =
  matchHeaderVersion (L8.pack "P5") *>> skipSpaces *>> getNum *>= \width ->
    skipSpaces *>> getNum *>= \height ->
      skipSpaces *>> getNum *>= \greyMax ->
        (if greyMax > 255
           then bail "Maximum grey value too high"
           else parseByte 1) *>>
        parseByte (width * height) *>= \bytes ->
          idParser (Greymap width height greyMax bytes)

----------------------
-- Helper functions --
----------------------
lst2num :: [Int] -> Int
lst2num = foldr 0 $ \(num, dig) -> num * 10 + dig

lst2bstr :: [Word8] -> L.ByteString
lst2bstr = undefined

----------------
-- Quick test --
----------------
test :: IO ()
test =
  putStrLn $
  show $
  doParse parseP5 $
  ParseState
    { contents =
        L8.pack
          "P5\n10 1\n200\nexample bytestring which is really not a bytestring..."
    , offset = 0
    }
