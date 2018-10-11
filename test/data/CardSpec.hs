module Data.CardSpec(spec) where

import           Data.Card  (Card(..), fromString, toString)
import           Data.Translation(Translation(..))
import           Test.Hspec

spec :: Spec
spec = do
  describe "deserialization of Card" $ do
    it "should work with space separators" $ do
      fromString "a b 1" `shouldBe` Just  (Card (Translation "a" "b") 1)
      
    it "should work with multiple space separators" $ do
      fromString "a      b     1 any" `shouldBe` Just  (Card (Translation "a" "b") 1)
      
    it "should work with tab separators" $ do
      fromString "a \t\t  b \t\t 1 \t\t ad"  `shouldBe` Just  (Card (Translation "a" "b") 1)
      
    it "should fail if incomplete string" $ do
      fromString "a b" `shouldBe` Nothing
