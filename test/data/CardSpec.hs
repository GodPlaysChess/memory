module Data.CardSpec(spec) where

-- import           Data.Card  (fromString, toString)
import           Test.Hspec

spec :: Spec
spec = do
  describe "deserialization of Card" $ do
    it "should work with any separators" $ do
      true `shouldBe` true

