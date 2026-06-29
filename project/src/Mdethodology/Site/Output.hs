module Mdethodology.Site.Output (applyTemplates, writeOutput) where

import Mdethodology.Types
import Mdethodology.Site.Render (renderBlocks)
import Mdethodology.Site.Template (renderTemplate)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), takeDirectory)
import qualified Data.Map as Map

-- For each page, render its body and stash the HTML in the page's template field
-- (kept simple here: we render straight to a string and store it for writeOutput).
applyTemplates :: FilePath -> Pass
applyTemplates templ site = pure site   -- wiring left intentionally light; see exercise

writeOutput :: FilePath -> Pass
writeOutput outDir site = do
  mapM_ writeOne (sitePages site)
  pure site
  where
    writeOne (Route r, page) = do
      let body  = renderBlocks (srcBody (pageSource page))
          scope = Map.fromList [("content", body), ("title", titleOf page)]
          file  = outDir </> dropLeadingSlash r <.> "html"
      case renderTemplate scope defaultTemplate of
        Right html -> do
          createDirectoryIfMissing True (takeDirectory file)
          writeFile file html
        Left err   -> ioError (userError (show err))
    titleOf _ = "Untitled"          -- pull from frontmatter as an exercise
    dropLeadingSlash ('/':rest) = if null rest then "index" else rest
    dropLeadingSlash s          = s
    defaultTemplate =
      "<!doctype html><html><head><title>{{title}}</title></head>\
      \<body>{{content}}</body></html>"
