module Data.Env(Env(..)) where

data Env = Env {
  inputPath   :: String
  , storePath :: String
  }
