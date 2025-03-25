{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib where

import Control.Monad (void)
import Control.Monad.Extra (when)
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (symbol)

type Error = String
type Input = Text

{- | @Parser a@ is a parser producing a value of type @a@. The first @Text@
is the type of error (so unstructured) while the second @Text@ is the type
of input the parser accepts.
-}
type Parser = Parsec Error Input

-- | A symbol in the map of an ASCII roguelike
data Symbol
  = -- | A wall, depicted by a # character
    Wall
  | -- | A floor, depicted by a . character
    Floor
  | -- | A water cell, depicted by a ~ character
    Water
  deriving (Eq, Show)

data Element
  = -- | Horizontal line, starting at @(x,y)@ with @length@ cells (ending at @(x+length-1,y)@)
    HorizontalLine Int Int Int
  | -- | Vertical line, starting at @(x,y)@ with @length@ cells (ending at @(x,y+length-1)@)
    VerticalLine Int Int Int
  | -- | A cell at @(x,y)@ with a symbol
    Cell Int Int Symbol
  | -- | The starting point of the player
    Start Int Int
  deriving (Eq, Show)

-- | Instance required for 'runMyParser'
instance ShowErrorComponent Error where
  showErrorComponent = id

{- | A variant of megaparsec's 'runParser', instantiated to our context.
Successfully parses an @a@ or returns an 'Error'.
-}
runMyParser :: Parser a -> Input -> Either Error a
runMyParser parser url =
  case runParser parser "" url of
    Left err -> Left $ errorBundlePretty err
    Right x -> Right x

parseSymbol :: Parser Symbol
parseSymbol = do
  c <- anySingle
  case c of
    '#' -> return Wall
    '.' -> return Floor
    '~' -> return Water
    _ -> fail $ "Unknown symbol: " <> [c] -- See https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec.html#v:failure for reporting the possible cases

parseLineElement :: Parser Element
parseLineElement = do
  constructor <- choice [char 'H' >> return HorizontalLine, char 'V' >> return VerticalLine]
  void $ string "Line"
  space1 -- One or more space
  x <- decimal
  space1
  y <- decimal
  space1
  len <- decimal
  when (len < 1) $ fail $ "Length must be greater than 0, but got " <> show len
  return $ constructor x y len

parseElement :: Parser Element
parseElement = choice [parseLineElement, parseStart, parseCell]
 where
  parseStart = do
    void $ string "Start"
    space1
    x <- decimal
    space1
    y <- decimal
    return $ Start x y
  parseCell = do
    void $ string "Cell"
    space1
    x <- decimal
    space1
    y <- decimal
    space1
    symbol <- parseSymbol
    return $ Cell x y symbol

parseElements :: Parser [Element]
parseElements = parseElement `sepBy1` separator
 where
  separator = do
    hspace -- Optional horizontal (non-newline) space
    choice [void $ char ';', void $ some eol] -- Either a single ';' or many newlines
    hspace