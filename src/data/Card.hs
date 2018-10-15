module Data.Card (Card(..)
                 , toString
                 , createFreshCard) where

import           Control.Arrow    (first)
import           Data.Foldable    (toList)
import qualified Data.Translation as T (Translation (..), toString)
import           Text.Read        (readMaybe)

data Card = Card T.Translation Int deriving (Show, Eq)

instance Read Card where
  readsPrec _ str =  case words str of
    e : r : i : rest -> toList $ (\i -> (Card (T.Translation e r) i, unwords rest)) <$> (readMaybe i)
    _            -> []

toString :: Card -> String
toString (Card t i) = T.toString t ++ show i

createFreshCard :: T.Translation -> Card
createFreshCard t = Card t 0
