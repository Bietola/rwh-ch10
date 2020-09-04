module Lib where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Data.Char (isSpace)

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
  show (Greymap w h m b) =
    -- "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m ++ " [...]"
    "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m ++ ": " ++ show b

---------------------
-- Parsing Library --
---------------------
type ParseResult a = Maybe (a, L.ByteString)

newtype Parser a =
  Parser
    { doParse :: L.ByteString -> ParseResult a
    }

-- parseNum >>= (\num ->
--   skipSpaces >>= (\() ->
--     parseNum >>= (\num2 -> 
--       return (num, num2))))
(*>=) :: Parser a -> (a -> Parser b) -> Parser b
parser *>= parserGen =
  Parser $ \input ->
    case (doParse parser input) of
      Nothing -> Nothing
      Just (output, input) -> doParse (parserGen output) input

(*>>) :: Parser a -> Parser b -> Parser b
parser *>> otherParser = parser *>= \(_) -> otherParser

-- TODO: Try to remove $
idParser :: a -> Parser a
idParser val = Parser $ \input -> Just (val, input)

bail :: Parser a
bail = Parser $ \_ -> Nothing

matchHeaderVersion :: L.ByteString -> Parser ()
matchHeaderVersion version =
  Parser $ \input ->
    if version `L8.isPrefixOf` input
      then Just $ ((), L.drop (fromIntegral (L.length version)) input)
      else Nothing

getNum :: Parser Int
getNum =
  Parser $ \input ->
    case L8.readInt input of
      Nothing -> Nothing
      Just (num, rest) ->
        if num < 0
          then Nothing
          else Just (fromIntegral num, rest)

getBytes :: Int -> Parser L.ByteString
getBytes amount =
  Parser $ \input ->
    let amount64 = fromIntegral amount
     in if L.length input < amount64
          then Nothing
          else Just $ L.splitAt amount64 input

skipSpaces :: Parser ()
skipSpaces = Parser $ \input -> Just ((), L8.dropWhile isSpace input)

parseP5 :: Parser Greymap
parseP5 =
  matchHeaderVersion (L8.pack "P5") *>> skipSpaces *>> getNum *>= \width ->
    skipSpaces *>> getNum *>= \height ->
      skipSpaces *>> getNum *>= \greyMax ->
        (if greyMax > 255
           then bail
           else getBytes 1) *>>
        getBytes (width * height) *>= \bytes ->
          idParser (Greymap width height greyMax bytes)

test :: IO ()
test =
  putStrLn $
  show $
  doParse parseP5 $
  L8.pack
    "P5\n10 1\n200\nexample bytestring which is really not a bytestring..."
