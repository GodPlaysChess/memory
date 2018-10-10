module Data.CardSpec(spec) where

import           Data.Card  (fromString, toString)
import           Test.Hspec

spec :: Spec
spec = do
  describe "deserialization of Card" $ do
    it "should work with any separators" $ do
      fromString "a b 1" `shouldBe` Just  (Card "a" "b" 1)
      fromString "a      b     1 any" `shouldBe` Just  (Card "a" "b" 1)
      fromString "a \t\t  b \t\t 1 \t\t ad"  `shouldBe` Just  (Card "a" "b" 1)
      fromString "a b" `shouldBe` Nothing
