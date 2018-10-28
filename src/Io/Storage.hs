{-# LANGUAGE OverloadedStrings #-}

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

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BS (pack, readFile, writeFile)
import           Data.Text              (Text, pack)
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import qualified Data.Translation       as T (Translation (..),
                                              translationParser)
import           Text.Megaparsec        (parseMaybe, sepEndBy)
import           Text.Megaparsec.Char   (eol)

type App a = ReaderT Env IO a


saveToFile :: [Card] -> App ()
saveToFile cards = (reader storePath) >>= (\s ->
                     liftIO . BS.writeFile s . encodeUtf8 . pack . unlines $ toString <$> cards)


readTranslation :: App [T.Translation]
readTranslation = do
  inputP <- reader inputPath
  content <- liftIO $ readFileUtf8 inputP
  liftIO $ putStrLn (show content)
  return $ fold $ parseMaybe (T.translationParser `sepEndBy` eol) content


readCards :: App [Card]
readCards = do
  inputP <- reader storePath
  content <- liftIO $ readFileUtf8 inputP
  return $ fold $ parseMaybe (cardParser `sepEndBy` eol) content


addTranslations :: App ()
addTranslations = do
  ts <- readTranslation
  liftIO $ putStrLn $ "Debug: " ++ show ts
  let cs = (toString . createFreshCard) <$> ts
      content = intercalate "\n" cs
  out <- reader storePath
  lift $ appendFile out content


readFileUtf8 :: FilePath -> IO Text
readFileUtf8 fp = decodeUtf8 <$> BS.readFile fp
