{-# LANGUAGE OverloadedStrings #-}


module Dialogue(dialogue) where

import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader      (ReaderT, ask, lift, mapReaderT,
                                            reader, withReaderT)
import           Control.Monad.Trans.Class (lift)
import           Data.Card                 (Card (..), cardParser,
                                            createFreshCard, toString)
import           Data.Env                  (Env (..))
import qualified Data.Env                  as Env (inputPath, storePath)
import           Data.Maybe                (listToMaybe)
import qualified Data.Translation          as T (Translation (..),
                                                 translationParser)
import           Debug.Trace
import           Io.Util
import           ListT                     (ListT, fold, fromFoldable, toList)
import           System.Environment        (getArgs)
import           System.Random.Shuffle     (shuffleM)

type App a = ReaderT Env IO a

dialogue :: App ()
dialogue = do
  addTranslations
  cards <- readCards
  lift $ putStrLn "Press (:e) to exit"
  shuffledCards <- liftIO $ shuffleM cards
  startGame shuffledCards

startGame :: [Card] -> App ()
startGame [] = lift $ putStrLn "You've learnt everything"
startGame cs@((Card t@(T.Translation en ru) i) : rest) = do
  lift $ putStrLn $ "how to translate" ++ en ++ "?"
  guess <- lift getLine
  case guess of
    g | g == ru -> (lift $ putStrLn "Correct") *> (startGame $ rest ++ [Card t (i + 1)])
      | g == ":e" -> (lift $ putStrLn "ByeBye") *> saveToFile cs
      | otherwise -> (lift $ putStrLn "Incorrect") *> (startGame $ rest ++ [Card t 0])


addTranslations :: App ()
addTranslations = let readerFile = mapReaderT
                                     (fold (\s a -> pure (s ++ "\n" ++ a)) "")
                                     (do
                                         inTranslation <- readInputTrans
--                                         liftIO $ putTraceMsg (show inTranslation) -- debug
                                         return $ (toString . createFreshCard) $ trace (show inTranslation) inTranslation )
                  in
                    do
                      content <- readerFile
                      out <- reader Env.storePath
                      lift $ appendFile out content


saveToFile :: [Card] -> App ()
saveToFile cards = (reader Env.storePath) >>= (\s ->
                     lift . writeFile s . unlines $ toString <$> cards)


readInputTrans :: ReaderT Env (ListT IO) T.Translation
readInputTrans = do
  input <- reader Env.inputPath
  lift $ parseFromFile T.translationParser input-- log that file does not exist


readCards :: App [Card]
readCards = (reader Env.storePath) >>= (lift . toList . (parseFromFile cardParser))



