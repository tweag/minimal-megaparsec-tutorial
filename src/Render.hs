module Render where

import Lib
import Control.Monad.Extra (forM_)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Set (Set)

data Direction =
    DirUp
  | DirDown
  | DirLeft
  | DirRight
  deriving (Eq, Show, Enum, Bounded)

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
    starts = Set.fromList $ mapMaybe (\e -> case e of Start x y -> Just (x, y); _ -> Nothing) elements
    reachableCells = Set.foldr (\start acc -> Set.union acc (reachableFromStart start elements)) Set.empty starts

type CellsSet = Set (Int, Int)

reachableFromStart :: (Int, Int) -> [Element] -> CellsSet 
reachableFromStart start elements =
  go (Set.singleton start) (Set.singleton start)
  where
    size = computeWidthAndHeight elements
    go ::  CellsSet -> CellsSet -> CellsSet 
    go visited toVisit
      | null toVisit = visited
      | _ =
        let cell = Set.findMin toVisit
            newToVisit = Set.delete cell toVisit in
        if not (inMap size cell) || Set.member cell visited
      | not (inMap size toVisit) = visited
      | otherwise = go newVisited newToVisit
      where
        newVisited = union visited toVisit
        newToVisit = foldl (\acc cell -> union acc (reachableCells cell)) empty
        reachableCells :: (Int, Int) -> CellsSet 
        reachableCells cell = Set.fromList [moveFrom cell dir | dir <- [DirUp .. DirRight]]

inMap :: (Int, Int) -> (Int, Int) -> Bool
inMap (width, height) (x, y) = x >= 0 && x < width && y >= 0 && y < height

moveFrom :: (Int, Int) -> Direction -> (Int, Int)
moveFrom (x, y) =
  \case
    DirUp -> (x, y - 1)
    DirDown -> (x, y + 1)
    DirLeft -> (x - 1, y)
    DirRight -> (x + 1, y)

renderCell :: [Element] -> (Int, Int) -> Maybe Char
renderCell elements cell =
  go $ reverse elements
  where
    go :: [Element] -> Maybe Char
    go [] = Nothing
    go (e:_) | elementContains e cell =
      case e of
        Start _ _ -> Just '>'
        Cell _ _ s -> Just $ symbolToChar s
        _ -> Just '#'
    go (_:es) = go es

elementContains :: Element -> (Int, Int) -> Bool
elementContains element (cx, cy) =
  case element of
    HorizontalLine x y l -> (x <= cx && cx < x + l) && (y == cy)
    VerticalLine x y l -> (x == cx) && (y <= cy && cy < y + l)
    Cell x y _ -> (x == cx) && (y == cy)
    Start x y -> (x == cx) && (y == cy)

computeWidthAndHeight :: [Element] -> (Int, Int)
computeWidthAndHeight =
  foldl (\(w, h) e -> case e of
    HorizontalLine x _ l -> (max w (x + l), h)
    VerticalLine _ y l -> (w, max h (y + l))
    Cell x y _ -> (max w x, max h y)
    Start x y -> (max w x, max h y)) (0, 0)

symbolToChar :: Symbol -> Char
symbolToChar = \case
  Wall -> '#'
  Floor -> '.'
  Water -> '~'