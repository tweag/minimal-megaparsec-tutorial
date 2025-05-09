{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (isLeft)
import Lib
import Render
import Test.Hspec

-- | Run me with @cabal test min-mega-test@
main :: IO ()
main = hspec $ do
  describe "parse" $ do
    it "parseSymbol" $ do
      runMyParser parseSymbol "#" `shouldBe` (Right Wall)
      runMyParser parseSymbol "~" `shouldBe` (Right Water)

      runMyParser parseSymbol "" `shouldSatisfy` isLeft
      runMyParser parseSymbol "a" `shouldSatisfy` isLeft
    it "parseLineElement" $ do
      runMyParser parseLineElement "HLine 0 0 1" `shouldBe` (Right $ HorizontalLine 0 0 1)
      runMyParser parseLineElement "HLine   0   0      1" `shouldBe` (Right $ HorizontalLine 0 0 1)
      runMyParser parseLineElement "VLine 22 0 1" `shouldBe` (Right $ VerticalLine 22 0 1)

      runMyParser parseLineElement "WrongLine 0 0 1" `shouldSatisfy` isLeft
      runMyParser parseLineElement "HLine0 0 1" `shouldSatisfy` isLeft
      runMyParser parseLineElement "HLine 0 0 0" `shouldSatisfy` isLeft
      runMyParser parseLineElement "HLine 0 0 0" `shouldSatisfy` isLeft
    it "parseElement" $ do
      runMyParser parseElement "HLine 0 0 1" `shouldBe` (Right $ HorizontalLine 0 0 1)
      runMyParser parseElement "HLine   0   0      1" `shouldBe` (Right $ HorizontalLine 0 0 1)
      runMyParser parseElement "VLine 0 0 1" `shouldBe` (Right $ VerticalLine 0 0 1)
      runMyParser parseElement "Start 0 0" `shouldBe` (Right $ Start 0 0)
      runMyParser parseElement "Cell 0 0 #" `shouldBe` (Right $ Cell 0 0 Wall)
      runMyParser parseElement "Cell 10 0 #" `shouldBe` (Right $ Cell 10 0 Wall)

      runMyParser parseElement "Cell 0 0 +" `shouldSatisfy` isLeft
    it "parseElements" $ do
      runMyParser parseElements "HLine 0 0 1" `shouldBe` (Right $ [HorizontalLine 0 0 1])
      runMyParser parseElements "VLine 0 0 1" `shouldBe` (Right $ [VerticalLine 0 0 1])
      runMyParser parseElements "HLine 0 0 1;VLine 0 0 1" `shouldBe` (Right $ [HorizontalLine 0 0 1, VerticalLine 0 0 1])
      runMyParser parseElements "HLine 0 0 1 ; VLine 0 0 1" `shouldBe` (Right $ [HorizontalLine 0 0 1, VerticalLine 0 0 1])
      runMyParser parseElements "HLine 0 0 1; VLine 0 0 1" `shouldBe` (Right $ [HorizontalLine 0 0 1, VerticalLine 0 0 1])
      runMyParser parseElements "HLine 0 0 1  ;  VLine 0 0 1" `shouldBe` (Right $ [HorizontalLine 0 0 1, VerticalLine 0 0 1])
      runMyParser parseElements "HLine 0 0 1\nVLine 0 0 1" `shouldBe` (Right $ [HorizontalLine 0 0 1, VerticalLine 0 0 1])
      runMyParser parseElements "HLine 0 0 1\nVLine 0 0 1;HLine 2 2 1" `shouldBe` (Right $ [HorizontalLine 0 0 1, VerticalLine 0 0 1, HorizontalLine 2 2 1])
      runMyParser parseElements "HLine 0 0 1\n\nVLine 0 0 1" `shouldBe` (Right $ [HorizontalLine 0 0 1, VerticalLine 0 0 1])

      runMyParser parseElements "HLine 0 0 1;;VLine 0 0 1" `shouldSatisfy` isLeft
  describe "render" $ do
    it "render" $ do
      computeWidthAndHeight [Cell 0 0 Wall] `shouldBe` (0, 0)
      computeWidthAndHeight [HorizontalLine 0 0 1] `shouldBe` (0, 0)
      computeWidthAndHeight [VerticalLine 0 0 3] `shouldBe` (0, 2)
      computeWidthAndHeight [HorizontalLine 0 0 1, VerticalLine 0 0 1] `shouldBe` (0, 0)
      computeWidthAndHeight [HorizontalLine 0 0 3] `shouldBe` (2, 0)
      computeWidthAndHeight [HorizontalLine 0 0 3, VerticalLine 0 0 2] `shouldBe` (2, 1)
