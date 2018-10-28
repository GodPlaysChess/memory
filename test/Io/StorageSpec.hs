module Io.StorageSpec (spec) where

import           Control.Monad.Reader (runReaderT)
import           Data.Card            (Card (..))
import           Data.Env             (Env (..))
import           Data.Text            (pack)
import qualified Data.Text.IO         as T (readFile)
import           Data.Translation     (Translation (..))
import           Io.Storage
import           Test.Hspec


spec :: Spec
spec = do
  describe "Storage" $ do
    it "should parse the english file" $ do
      runEngTest readTranslation `shouldReturn` [Translation "word" "translation", Translation "black" "white"]


    it "should parse the unicode file" $ do
      runUnicodeTest readTranslation `shouldReturn` [Translation "bleak" "мрачный", Translation "slouch" "сутулый"]

    it "should append correct date to the output" $ do
      pending


runUnicodeTest m = runReaderT m (Env testUnicodeInput testOutput)
runEngTest m = runReaderT m (Env testEngInput testOutput)



testEngInput     = "d:\\dev\\prj\\haskell\\memory\\test\\resources\\test-eng"
testUnicodeInput = "d:\\dev\\prj\\haskell\\memory\\test\\resources\\test-in"
testOutput       = "d:\\dev\\prj\\haskell\\memory\\test\\resources\\test-out"

