module Io.Storage (
  saveToFile
  , readTranslation
  , readCards
  , addTranslations
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, ask, lift, reader)
import           Data.Card              (Card (..), cardParser, createFreshCard,
                                         toString)

import           Data.Env               (Env (..), inputPath, storePath)
import           Data.Foldable          (fold)
import           Data.List              (intercalate)

import qualified Data.Translation       as T (Translation (..),
                                              translationParser)
import           Text.Parsec.Char       (endOfLine)
import           Text.Parsec.Combinator (sepEndBy)
import           Text.Parsec.String     (parseFromFile)

type App a = ReaderT Env IO a


saveToFile :: [Card] -> App ()
saveToFile cards = (reader storePath) >>= (\s ->
                     lift . writeFile s . unlines $ toString <$> cards)


readTranslation :: App [T.Translation]
readTranslation = lift . (fmap fold) . parseFromFile (T.translationParser `sepEndBy` endOfLine) =<<
             (reader inputPath)


readCards :: App [Card]
readCards = lift . (fmap fold) . parseFromFile (cardParser `sepEndBy` endOfLine) =<<
             (reader storePath)


addTranslations :: App ()
addTranslations = do
  ts <- readTranslation
  liftIO $ putStrLn $ "Debug: " ++ show ts
  let cs = (toString . createFreshCard) <$> ts
      content = intercalate "\n" cs
  out <- reader storePath
  lift $ appendFile out content
