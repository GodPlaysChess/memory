module Data.Translation (Translation (..)
                        , toString
                        , translationParser
                        ) where

import           Text.Parsec.Char       (letter)
import           Text.Parsec.Char       (space)
import           Text.Parsec.Combinator (many1, skipMany1)
import           Text.Parsec.String     (Parser)


data Translation = Translation {
  en   ::  String
  , ru :: String
  } deriving (Show, Eq)


translationParser :: Parser Translation
translationParser = do
  en <- many1 letter
  skipMany1 space
  ru <- many1 letter
  return $ Translation en ru


toString :: Translation -> String
toString tr = en tr ++ ru tr
