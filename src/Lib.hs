{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib where

import Control.Monad (void)
import Data.Text (Text)
import Text.Megaparsec hiding (runParser)
import Text.Megaparsec qualified as Mega
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Error = String
type Input = Text

{- | @Parser a@ is a parser producing a value of type @a@. The first @Text@
is the type of error (so unstructured) while the second @Text@ is the type
of input the parser accepts.
-}
type Parser = Parsec Error Input

-- | A coordinate
data Coord = Coord Int Int
  deriving (Eq, Show)

-- | A symbol in the map of an ASCII roguelike
data Symbol
  = -- | A wall, depicted by a # character
    Wall
  | -- | A floor, depicted by a . character
    Floor
  | -- | A water cell, depicted by a ~ character
    Water
  deriving (Eq, Show)

data Shape
  = Rectangle
  | Circle

data RectangleRoom
  = RectangleRoom
  { roomCoord :: Coord
  -- ^ Top-left corner of the room
  , roomWidth :: Int
  , roomHeight :: Int
  }

data CircleRoom
  = CircleRoom
  { roomCoord :: Coord
  -- ^ Center of the room
  , roomRadius :: Int
  }

-- | Instance required for 'runParser'
instance ShowErrorComponent String where
  showErrorComponent = id

{- | A variant of megaparsec's 'runParser', instantiated to to our context.
Successfully parses a @b@ or returns an 'Error'.
-}
runParser :: Parser b -> Input -> Either Error b
runParser parser url =
  case Mega.runParser parser "" url of
    Left err -> Left $ errorBundlePretty err
    Right x -> Right x

parseCoord :: Parser Coord
parseCoord = do
  void $ string "Coord"
  space1 -- At least one space
  x :: Int <- decimal -- See also signed: https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Byte-Lexer.html#v:signed
  space1
  y <- decimal
  return $ Coord x y

parseSymbol :: Parser Symbol
parseSymbol = do
  c <- anySingle
  case c of
    '#' -> return Wall
    '.' -> return Floor
    '~' -> return Water
    _ -> fail $ "Unknown symbol: " <> [c] -- See https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec.html#v:failure for reporting the possible cases