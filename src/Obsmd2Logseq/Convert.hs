module Obsmd2Logseq.Convert
    ( migrate
    ) where

import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Either (fromRight)
import Text.Regex.Applicative (RE (..), replace, (<|>), (<*>), many, string, psym, sym, match)
import Text.Regex.Applicative.Common (digit)
import Data.Maybe (catMaybes, isJust)
import System.FilePath ((</>), takeFileName)

import Obsmd2Logseq.Meta (transformMeta)
import Obsmd2Logseq.Settings

-- Convert a single file from Obsidian and copy it to the LogSeq vault
migrate :: Settings -> FilePath -> IO Bool
migrate settings filepath = do
  putStrLn $ "Converting " ++ filepath
  let filename = takeFileName filepath
  let subfolder = if isJournalFile filename then "journals" else "pages"

  content <- readFile filepath

  -- Exclude empty files
  if content == "" then return False
  else do

    -- Transform the content
    resultOrError <- runIO $ transform (T.pack content)

    -- Catch Pandoc errors and just print them
    case resultOrError of
      Left error -> do
        putStrLn $ T.unpack $ renderError error
        return False

      Right result -> do

        -- Some post-rendering fixes: un-espace stuff using regexes
        -- * Wikilinks (soon to be supported, see https://github.com/jgm/pandoc/pull/7705)
        -- * Backquotes (e.g. \`something\` -> `something`)
        -- * Hashtags (e.g. \#evergreen -> #evergreen)
        let regexps = reWikilink <|> reBackquotes <|> reHashtag
        let result' = replace regexps (T.unpack result)

        -- Finally write the converted Markdown to the LogSeq vault
        let destFilepath = (pathToLogSeqVault settings) </> subfolder </> filename
        writeFile destFilepath result'
        return True

-- Using Pandom to parse, transform and re-render Markdown content
transform :: T.Text -> PandocIO T.Text
transform text = do
    -- Parse into Pandoc's representation
    doc <- readMarkdown readerOptions text
    -- Move metadata from the YAML header (Obsidian)
    -- to the beginning of the Markdown file (LogSeq)
    let doc' = transformMeta doc
    -- Render back to Markdown
    writeMarkdown writerOptions doc'

readerOptions :: ReaderOptions
readerOptions =
  (def :: ReaderOptions)
  {readerExtensions =
    enableExtension Ext_raw_attribute $
    disableExtension Ext_citations $
    enableExtension Ext_yaml_metadata_block strictExtensions}

writerOptions :: WriterOptions
writerOptions =
  (def :: WriterOptions)
  {writerExtensions = strictExtensions}

isJournalFile :: String -> Bool
isJournalFile filename =
  isJust $ match reJournalFile filename

-- \[\[Note\]\] -> [[Note]]
reWikilink :: RE Char [Char]
reWikilink = ((\x -> "[[" ++ x ++ "]]") <$> (string "\\[\\[" *> (many $ psym $ (/= ']')) <* string "\\]\\]") )

-- \`something\` -> `something`
reBackquotes :: RE Char [Char]
reBackquotes = ((\x -> "`" ++ x ++ "`") <$> (string "\\`" *> (many $ psym $ (/= '`')) <* string "\\`") )

-- \#evergreen -> #evergreen
reHashtag :: RE Char [Char]
reHashtag = (\x -> "#" ++ [x]) <$> (sym '\\' *> sym '#' *> psym (/= ' '))

-- Matches YYYY-MM-DD.md
reJournalFile =
  digit <* digit <* digit <* digit <* sym '-' <* digit <* digit <* sym '-' <* digit <* digit <* string ".md"
