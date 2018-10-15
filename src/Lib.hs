module Lib(someFunc) where

import           Control.Monad.Reader (runReaderT)
import           Data.Env             (Env (..))
import           Dialogue             (dialogue)

someFunc :: IO ()
someFunc = runReaderT dialogue (Env inputPathE storePathE)

storePathE :: FilePath
storePathE = "./store/words"

inputPathE :: FilePath
inputPathE = "./store/in.txt"

