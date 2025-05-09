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
    widths = [0 .. width - 1]
    heights = [0 .. height - 1]

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
        _ -> Just '#'
    go (_ : es) = go es

elementContains :: Element -> (Int, Int) -> Bool
elementContains element (cx, cy) =
  case element of
    HorizontalLine x y l -> (x <= cx && cx < x + l) && (y == cy)
    VerticalLine x y l -> (x == cx) && (y <= cy && cy < y + l)
    Cell x y _ -> (x == cx) && (y == cy)
    Start x y -> (x == cx) && (y == cy)

computeWidthAndHeight :: [Element] -> (Int, Int)
computeWidthAndHeight =
  foldl
    ( \(w, h) e -> case e of
        HorizontalLine x y l -> (max w (x + l), max h (y + 1))
        VerticalLine x y l -> (max w (x + 1), max h (y + l))
        Cell x y _ -> (max w (x + 1), max h (y + 1))
        Start x y -> (max w (x + 1), max h (y + 1))
    )
    (0, 0)

symbolToChar :: Symbol -> Char
symbolToChar = \case
  Wall -> '#'
  Water -> '~'
