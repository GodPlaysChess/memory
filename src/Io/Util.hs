module Io.Util(
  parseFromFile
  ) where

import           Control.Monad.IO.Class (liftIO)
import           ListT                  (ListT, fromFoldable)
import           Text.Read              (readMaybe)


parseFromFile :: Read a => FilePath -> ListT IO a
parseFromFile path = do
  file <- liftIO $ readFile path
  line <- fromFoldable $ lines file
  translation <- fromFoldable . readMaybe $ line
  return translation
