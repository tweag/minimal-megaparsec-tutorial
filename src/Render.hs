module Render where

import Control.Monad.Extra (forM_)
import Data.Maybe (fromMaybe)
import Lib

render :: [Element] -> IO ()
render elements = do
  forM_ [y | y <- heights] $ \y -> do
    forM_ [x | x <- widths] $ \x -> do
      let cell = (x, y)
          symbol = renderCell elements cell
      putChar (fromMaybe ' ' symbol)
    putStrLn ""
  where
    (width, height) = computeWidthAndHeight elements
    widths = [0 .. width]
    heights = [0 .. height]

renderCell :: [Element] -> (Int, Int) -> Maybe Char
renderCell elements cell =
  go $ reverse elements
  where
    go :: [Element] -> Maybe Char
    go [] = Nothing
    go (e : _) | elementContains e cell =
      case e of
        Start _ _ -> Just '>'
        Cell _ _ s -> Just $ symbolToChar s
        HorizontalLine {} -> Just '#'
        VerticalLine {} -> Just '#'
    go (_ : es) = go es

elementContains :: Element -> (Int, Int) -> Bool
elementContains element (cx, cy) =
  case element of
    HorizontalLine x y l -> (x <= cx && cx < x + l) && (y == cy)
    VerticalLine x y l -> (x == cx) && (y <= cy && cy < y + l)
    Cell x y _ -> (x == cx) && (y == cy)
    Start x y -> (x == cx) && (y == cy)

-- | Compute the width and height of the map. This is the maximum
-- size of the x and y coordinates of all elements. Iterating over [0..w]
-- and [0..h] will cover all elements.
computeWidthAndHeight :: [Element] -> (Int, Int)
computeWidthAndHeight =
  foldl
    ( \(w, h) e ->
        let mw = maxX e; mh = maxY e
         in (max w mw, max h mh)
    )
    (0, 0)

-- | Compute the maximum x coordinate of an element. Iterating over
-- [0..returned value] covers all elements.
maxX :: Element -> Int
maxX = \case
  HorizontalLine x _ l -> x + l - 1
  VerticalLine x _ _ -> x
  Cell x _ _ -> x
  Start x _ -> x

-- | Compute the maximum y coordinate of an element. Iterating over
-- [0..returned value] covers all elements.
maxY :: Element -> Int
maxY = \case
  HorizontalLine _ y _ -> y
  VerticalLine _ y l -> y + l - 1
  Cell _ y _ -> y
  Start _ y -> y

symbolToChar :: Symbol -> Char
symbolToChar = \case
  Wall -> '#'
  Water -> '~'
