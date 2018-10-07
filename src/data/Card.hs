module Data.Card (Card(..)
                 , fromString
                 , toString
                 , createFreshCard) where

import qualified Data.Translation as T (Translation (..), toString)
import           Text.Read        (readMaybe)

data Card = Card T.Translation Int

fromString :: String -> Maybe Card
fromString str = case words str of
  (e : r : i : xs) -> Card (T.Translation e r) <$> (readMaybe i)
  _                -> Nothing

toString :: Card -> String
toString (Card t i) = T.toString t ++ show i

createFreshCard :: T.Translation -> Card
createFreshCard t = Card t 0
