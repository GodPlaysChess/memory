module Lib
    (   someFunc
    ) where

import Dialogue(dialogue)
import Control.Monad.Reader(runReaderT)
import Data.Env(Env(..))

someFunc :: IO ()
someFunc = runReaderT dialogue (Env inputPathE storePathE)

storePathE :: FilePath
storePathE = "./store/words"

inputPathE :: FilePath
inputPathE = "./store/in.txt"

