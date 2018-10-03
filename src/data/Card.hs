module Data.Card (Card(..)
                 , fromString
                 , toString
                 , createFresh) where

import qualified Data.Translation as T (Translation (..), toString)
import           Text.Read        (readMaybe)

data Card = Card T.Translation Int

fromString :: String -> Maybe Card
fromString str = case words str of
  e : r : i : _ -> Card (T.Translation e r) <$> (readMaybe i)
  _             -> Nothing

toString :: Card -> String
toString (Card t i) = T.toString t ++ show i

createFresh :: T.Translation -> Card
createFresh t = Card t 0
