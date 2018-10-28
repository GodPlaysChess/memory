{-# LANGUAGE OverloadedStrings #-}

module Data.CardSpec(spec) where

import           Data.Card        (Card (..), cardParser, toString)
import           Data.Either      (isLeft, isRight)
import           Data.Translation (Translation (..), translationParser)
import           Test.Hspec
import           Text.Megaparsec  (parse)

spec :: Spec
spec = do
  describe "Card Parsers" $ do
    it "should work with space separators" $ do
      parse cardParser "test" "a b 1" `shouldBe` succCard

    it "should work with multiple space separators" $ do
      parse cardParser "test"  "a      b     1 any" `shouldBe` succCard

    it "should work with tab separators" $ do
      parse cardParser "test" "a \t\t  b \t\t 1 \t\t ad"  `shouldBe` succCard

    it "should fail if incomplete string" $ do
      parse cardParser "test" "a b" `shouldSatisfy` isLeft


succCard = Right (Card (Translation "a" "b") 1)

