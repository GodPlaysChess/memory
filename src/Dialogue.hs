{-# LANGUAGE OverloadedStrings #-}
module Dialogue where

import           Control.Arrow             ((&&&))
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Data.Card                 (Card (..), createFresh, fromString,
                                            toString)
import           Data.Maybe                (listToMaybe)
import qualified Data.Translation          as T (Translation (..), fromString)
import           ListT                     (ListT, fold, fromFoldable, toList)
import           System.Environment        (getArgs)

dialogue :: IO String
dialogue = do
  addTranslations
  cards <- readCards
  let (Card t@(T.Translation en ru) i, rest) = randomCard cards
  putStrLn (en)
  guess <- readLn
  cards1 <- if (guess == ru)
            then cards ++ [Card t (i + 1)] <$ putStrLn "Correct"
            else cards ++ [Card t 0] <$ putStrLn "Incorrect"
  return "exit"

addTranslations :: IO ()
addTranslations = do
   new <- fold (\s a -> pure (s ++ "\n" ++ a)) "" $
     (toString . createFresh) <$> readInputTrans
   appendFile storePath new

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

randomCard :: [Card] -> (Card, [Card])
randomCard =  head &&& tail

