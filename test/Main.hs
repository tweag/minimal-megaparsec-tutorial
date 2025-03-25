{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec

import Lib
import Data.Either (isLeft)

-- | Run me with @cabal test all@
main :: IO ()
main = hspec $ do
  describe "parse" $ do
    it "parseCoord" $ do
      runParser parseCoord "Coord 1 1" `shouldBe` (Right $ Coord 1 1)

      runParser parseCoord "Coord1 1" `shouldSatisfy` isLeft
      runParser parseCoord "Coord   -1  2  " `shouldSatisfy` isLeft
    it "parseSymbol" $ do
      runParser parseSymbol "#" `shouldBe` (Right Wall)
      runParser parseSymbol "." `shouldBe` (Right Floor)
      runParser parseSymbol "~" `shouldBe` (Right Water)

      runParser parseSymbol "" `shouldSatisfy` isLeft
      runParser parseSymbol "a" `shouldSatisfy` isLeft