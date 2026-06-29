module Mdethodology.Site.Routes (routeFor, routeForPage) where

import Mdethodology.Types
import System.FilePath (dropExtension, splitDirectories)
import Data.List (intercalate)
import qualified Data.Map as Map

-- "site/blog/intro.md"  ->  Route "/blog/intro"
-- "site/index.md"       ->  Route "/"
routeFor :: FilePath -> FilePath -> Route
routeFor root path =
  let rel    = drop (length (splitDirectories root)) (splitDirectories path)
      noExt  = dropExtension (intercalate "/" rel)
  in Route $ case noExt of
       "index" -> "/"
       p       -> "/" ++ p

-- Derive a page's route: an explicit `slug:` in frontmatter overrides the
-- path-derived route; otherwise fall back to `routeFor`.
routeForPage :: FilePath -> FilePath -> YamlValue -> Route
routeForPage root path fm = case slugOf fm of
  Just s  -> Route (ensureLeadingSlash s)
  Nothing -> routeFor root path
  where
    slugOf (YMap m) = case Map.lookup "slug" m of
                        Just (YString s) -> Just s
                        _                -> Nothing
    slugOf _        = Nothing
    ensureLeadingSlash s@('/':_) = s
    ensureLeadingSlash s         = '/' : s
