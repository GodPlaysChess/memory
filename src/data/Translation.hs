module Data.Translation (Translation (..)
                        , toString
                        ) where

import           GHC.Read

data Translation = Translation {
  en   ::  String
  , ru :: String
  } deriving (Show, Eq)

instance Read Translation where
  readsPrec _ str =  case words str of
    e : r : rest -> [(Translation e r, unwords rest)]
    _            -> []


-- fromString :: String -> Maybe Translation
-- fromString str =  case words str of
--     e : r : _ -> Just $ Translation e r
--     _         -> Nothing


toString :: Translation -> String
toString tr = en tr ++ ru tr
