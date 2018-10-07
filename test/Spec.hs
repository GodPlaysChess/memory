import           Test.Hspec

import qualified Data.CardSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "CardSpec"     Data.CardSpec.spec
