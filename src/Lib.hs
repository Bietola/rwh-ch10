module Lib where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Data.Char (isSpace)
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

---------------------
-- Parsing Library --
---------------------
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

-- (repeat 10 getByte) *>= \bytes ->
--   getState *>= \state ->
--     putState ParseState { offset = (offset state) + 10 } *>>
--       skipSpaces *>>
getState :: Parser ParseState
getState = Parser $ \state -> Right (state, state)

putState :: ParseState -> Parser ()
putState newState = Parser $ \oldState -> Right ((), newState)

getByte :: Parser Word8
getByte =
  getState *>= \initState ->
    case L.uncons (contents initState) of
      Nothing -> bail "Not enough bytes."
      Just (byte, rest) ->
        putState ParseState {contents = rest, offset = offset initState + 1} *>>
        idParser byte

-- TODO: Advance offset
matchHeaderVersion :: L.ByteString -> Parser ()
matchHeaderVersion version =
  Parser $ \initState ->
    let newState =
          ParseState
            { contents = L.drop (fromIntegral prefixLen) (contents initState)
            , offset = offset initState + prefixLen
            }
        prefixLen = fromIntegral $ L8.length version
     in if version `L8.isPrefixOf` (contents initState)
          then Right ((), newState)
          else Left $ "Invalid header version " ++ show version ++ "."

-- getNum :: Parser Int
-- getNum = list2num <$> zeroOrMore (pred isDigit)
getNum :: Parser Int
getNum = undefined

-- TODO: Make implementation more modular (see below)
getBytes :: Int -> Parser L.ByteString
getBytes amount = undefined

-- TODO: Make implementation more modular, e.g. skipSapces = oneOrMore (pred isSpace getByte)
skipSpaces :: Parser ()
skipSpaces = undefined

parseP5 :: Parser Greymap
parseP5 =
  matchHeaderVersion (L8.pack "P5") *>> skipSpaces *>> getNum *>= \width ->
    skipSpaces *>> getNum *>= \height ->
      skipSpaces *>> getNum *>= \greyMax ->
        (if greyMax > 255
           then bail "Maximum grey value too high"
           else getBytes 1) *>>
        getBytes (width * height) *>= \bytes ->
          idParser (Greymap width height greyMax bytes)

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
