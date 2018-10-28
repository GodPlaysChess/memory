{-# LANGUAGE OverloadedStrings #-}


module Dialogue(
  dialogue
  , App
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, lift)

import           Data.Card              (Card (..))
import           Data.Env               (Env (..))
import           Data.Translation       (Translation (..))
import           System.Random.Shuffle  (shuffleM)

import           Io.Storage             (addTranslations, readCards,
                                         readTranslation, saveToFile)

type App a = ReaderT Env IO a

dialogue :: App ()
dialogue = do
  addTranslations
  cards <- readCards
  liftIO $ putStrLn "Press (:e) to exit"
  shuffledCards <- liftIO $ shuffleM cards
  startGame shuffledCards


startGame :: [Card] -> App ()
startGame [] = lift $ putStrLn "You've learnt everything"
startGame cs@((Card t@(Translation en ru) i) : rest) = do
  lift $ putStrLn $ "how to translate" ++ en ++ "?"
  guess <- lift getLine
  case guess of
    g | g == ru -> (lift $ putStrLn "Correct") *> (startGame $ rest ++ [Card t (i + 1)])
      | g == ":e" -> (lift $ putStrLn "ByeBye") *> saveToFile cs
      | otherwise -> (lift $ putStrLn "Incorrect") *> (startGame $ rest ++ [Card t 0])
