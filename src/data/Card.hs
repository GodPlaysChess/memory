module Data.Card (Card(..)
                 , toString
                 , createFreshCard
                 , cardParser
                 ) where

import           Control.Arrow          (first)
import           Data.Foldable          (toList)
import qualified Data.Translation       as T (Translation (..), toString,
                                              translationParser)
import           Text.Parsec.Char       (digit, space)
import           Text.Parsec.Combinator (many1, skipMany1)
import           Text.Parsec.String     (Parser)

import           Text.Read              (read, readMaybe)


data Card = Card T.Translation Int deriving (Show, Eq)


cardParser :: Parser Card
cardParser = do
  tr <- T.translationParser
  skipMany1 space
  i <- many1 digit
  return $ Card tr (read i)


toString :: Card -> String
toString (Card t i) = T.toString t ++ show i


createFreshCard :: T.Translation -> Card
createFreshCard t = Card t 0
