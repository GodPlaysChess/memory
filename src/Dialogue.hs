{-# LANGUAGE OverloadedStrings #-}

module Dialogue(dialogue) where

import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Data.Card                 (Card (..), createFreshCard,
                                            fromString, toString)
import           Data.Maybe                (listToMaybe)
import qualified Data.Translation          as T (Translation (..), fromString)
import           ListT                     (ListT, fold, fromFoldable, toList)
import          System.Environment        (getArgs)
import System.Random.Shuffle(shuffleM)

dialogue :: IO ()
dialogue = do
  addTranslations
  cards <- readCards
  putStrLn "Press (:e) to exit"
  startGame =<< shuffleM cards

startGame :: [Card] -> IO ()
startGame [] = putStrLn "You've learnt everything"
startGame cs@((Card t@(T.Translation en ru) i) : rest) = do
  putStrLn $ "how to translate" ++ en ++ "?"
  guess <- getLine
  case guess of
    g | g == ru -> putStrLn "Correct" *> (startGame $ rest ++ [Card t (i + 1)])
      | g == ":e" -> putStrLn "ByeBye" *> saveToFile cs
      | otherwise -> putStrLn "Incorrect" *> (startGame $ rest ++ [Card t 0])

addTranslations :: IO ()
addTranslations = do
   new <- fold (\s a -> pure (s ++ "\n" ++ a)) "" $
     (toString . createFreshCard) <$> readInputTrans
   appendFile storePath new

saveToFile :: [Card] -> IO ()
saveToFile cards = writeFile storePath . unlines $ toString <$> cards

readInputTrans :: ListT IO T.Translation
readInputTrans = do
  inputArgs <- liftIO getArgs
  inputPath <- fromFoldable . listToMaybe $ inputArgs
  readFromFile inputPath T.fromString-- log that file does not exist

storePath :: FilePath
storePath = "../store/words"

readCards :: IO [Card]
readCards = toList $ readFromFile storePath fromString

readFromFile :: FilePath -> (String -> Maybe a) -> ListT IO a
readFromFile path parse = do
  file <- liftIO $ readFile path
  line <- fromFoldable $ lines file
  translation <- fromFoldable . parse $ line
  return translation
