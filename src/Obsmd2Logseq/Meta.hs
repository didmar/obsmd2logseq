module Obsmd2Logseq.Meta
    ( transformMeta
    ) where

import Text.Pandoc
import Text.Pandoc.Definition (MetaValue (..))
import qualified Data.Text as T
import qualified Data.Map.Internal as M
import Data.List (intersperse)
import Data.Maybe (mapMaybe)

transformMeta :: Pandoc -> Pandoc
transformMeta doc =
    let metaKVLst = toMetaKVList doc
        metaBlock = LineBlock $ mapMaybe metaKVToInlines metaKVLst
    in prependBlock doc metaBlock

prependBlock :: Pandoc -> Block -> Pandoc
prependBlock (Pandoc m blocks) newBlock =
  Pandoc m (newBlock : blocks)

toMetaKVList :: Pandoc -> [(T.Text, MetaValue)]
toMetaKVList (Pandoc m _ ) = M.assocs $ unMeta m

metaValueToInlines :: MetaValue -> [Inline]
metaValueToInlines metaValue =
  case metaValue of
    MetaString s -> [Str s]
    MetaInlines inlines -> inlines
    MetaList metaValues -> concatMap metaValueToInlines metaValues
    _ -> []

metaKVToInlines :: (T.Text, MetaValue) -> Maybe [Inline]
metaKVToInlines (key, metaValue) =
  let values = metaValueToInlines metaValue
      entry = [Str (key `T.append` (T.pack "::")), Space ] :: [Inline]
      commaSepareInlines inlines =
        concat $ intersperse [Str (T.pack ","), Space] $ map (\i -> [i]) inlines
  in case values of
      [] -> Nothing
      _ -> Just $ (entry ++ commaSepareInlines values)
