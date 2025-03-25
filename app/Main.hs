module Main where

import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Lib qualified
import Render qualified
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (stdin)
import Text.Megaparsec qualified as Parsec

main :: IO ()
main = do
  args <- getArgs
  (content, source) <- case args of
    ["--input-map", filepath] -> do
      result <- T.readFile filepath
      return (result, Just filepath)
    [] -> do
      result <- T.hGetContents stdin
      return (result, Nothing)
    _ -> die "Usage: ./min-mega-exe [--input-map <FILE>] (or pipe input to stdin)"
  case runParser source Lib.parseElements content of
    Left err -> die $ T.unpack $ "Error parsing map: " <> err
    Right elements -> do
      Render.render elements

runParser :: Maybe FilePath -> Lib.Parser a -> Lib.Input -> Either Lib.Error a
runParser filepath parser url =
  case Parsec.runParser parser (fromMaybe "" filepath) url of
    Left err -> Left $ T.pack $ Parsec.errorBundlePretty err
    Right x -> Right x
