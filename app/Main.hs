module Main where

import System.Environment (getArgs)

import Obsmd2Logseq.Run (run)
import Obsmd2Logseq.Settings

main :: IO ()
main = do
  args <- getArgs
  case args of
    (pathToObsidianVault:pathToLogSeqVault:_) ->
      run (Settings (pathToObsidianVault :: FilePath) (pathToLogSeqVault :: FilePath))
    _ -> showUsage

showUsage :: IO ()
showUsage =
  putStrLn "Usage: obsmd2logseq <PATH_TO_OBSIDIAN_VAULT> <PATH_TO_LOGSEQ_VAULT>"