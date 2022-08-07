module Obsmd2Logseq.Run
    ( run
    ) where

import System.FilePath (takeExtension)
import System.Directory.Extra (listFilesRecursive)

import Obsmd2Logseq.Convert (migrate)
import Obsmd2Logseq.Settings

run :: Settings -> IO ()
run settings = do
  -- Find all the Markdown files inside the Obsidian vault
  files <- listFilesRecursive (pathToObsidianVault settings)
  let mdFiles = filter isMDFile files
  -- Try to migrate each of them
  successes <- mapM (migrate settings) mdFiles
  -- Show some stats
  let nbFilesCopied = length $ filter id successes
  putStrLn $ "Copied " ++ show nbFilesCopied ++ " / " ++ (show $ length files) ++ " files"

isMDFile :: FilePath -> Bool
isMDFile filepath =
  takeExtension filepath == ".md"