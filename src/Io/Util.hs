module Io.Util(
  parseFromFile
) where

import           Control.Monad.IO.Class (liftIO)
import           ListT                  (ListT, fromFoldable)
import           Text.Parsec            (parse)
import           Text.Parsec.String     (Parser)


-- parseMultipleFromFile :: P.Parser a -> FilePath -> ListT IO a
-- parseMultipleFromFile = P.parseFromFile

parseFromFile :: Parser a -> FilePath -> ListT IO a
parseFromFile psr path = do
  file <- liftIO $ readFile path
  line <- fromFoldable $ lines file
  translation <- fromFoldable $ parse psr line line
  return translation
