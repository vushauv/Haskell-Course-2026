module Mdethodology.Site.Routes (routeFor) where

import Mdethodology.Types
import System.FilePath (dropExtension, splitDirectories)
import Data.List (intercalate)

-- "site/blog/intro.md"  ->  Route "/blog/intro"
-- "site/index.md"       ->  Route "/"
routeFor :: FilePath -> FilePath -> Route
routeFor root path =
  let rel    = drop (length (splitDirectories root)) (splitDirectories path)
      noExt  = dropExtension (intercalate "/" rel)
  in Route $ case noExt of
       "index" -> "/"
       p       -> "/" ++ p
