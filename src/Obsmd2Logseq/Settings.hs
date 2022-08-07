module Obsmd2Logseq.Settings
    ( Settings(..)
    ) where

data Settings = Settings
  { pathToObsidianVault :: FilePath
  , pathToLogSeqVault :: FilePath
  }