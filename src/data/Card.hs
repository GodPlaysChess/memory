module Data.Card (Card(..)
                 , toString
                 , createFreshCard
                 , cardParser
                 ) where

import           Data.Text            (Text)
import qualified Data.Translation     as T (Translation (..), toString,
                                            translationParser)
import           Data.Void
import           Text.Megaparsec      (Parsec, skipSome, some)
import           Text.Megaparsec.Char (digitChar, spaceChar)


data Card = Card T.Translation Int deriving (Show, Eq)


cardParser :: Parsec Void Text Card
cardParser = do
  tr <- T.translationParser
  skipSome spaceChar
  i <- some digitChar
  return $ Card tr (read i)


toString :: Card -> String
toString (Card t i) = T.toString t ++ show i


createFreshCard :: T.Translation -> Card
createFreshCard t = Card t 0
