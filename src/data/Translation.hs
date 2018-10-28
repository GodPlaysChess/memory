module Data.Translation (Translation (..)
                        , toString
                        , translationParser
                        ) where

import           Data.Text            (Text)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char (letterChar, spaceChar)

data Translation = Translation {
  en   ::  String
  , ru :: String
  } deriving (Show, Eq)


translationParser :: Parsec Void Text Translation
translationParser = do
  en <- some letterChar
  skipSome spaceChar
  ru <- some letterChar
  return $ Translation en ru


toString :: Translation -> String
toString tr = en tr ++ ru tr
