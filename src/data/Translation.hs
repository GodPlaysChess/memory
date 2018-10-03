module Data.Translation (Translation (..)
                        , toString
                        , fromString) where

data Translation = Translation {
  en   ::  String
  , ru :: String
  }

fromString :: String -> Maybe Translation
fromString str = case words str of
  e : r : _ -> Just $ Translation e r
  _         -> Nothing

toString :: Translation -> String
toString tr = en tr ++ ru tr
