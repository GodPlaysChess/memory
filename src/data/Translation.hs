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
    e : r : rest -> [(Translation e r, "")]
    _            -> []


toString :: Translation -> String
toString tr = en tr ++ ru tr
