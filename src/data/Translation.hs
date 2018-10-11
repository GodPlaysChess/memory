module Data.Translation (Translation (..)
                        , toString
                        , fromString) where

data Translation = Translation {
  en   ::  String
  , ru :: String
  } deriving (Show, Eq)

fromString :: String -> Maybe Translation
fromString _ = Nothing
-- fromString str = case words str of
  -- e : r : _ -> Just $ Translation e r
  -- _         -> Nothing

toString :: Translation -> String
toString tr = en tr ++ ru tr
