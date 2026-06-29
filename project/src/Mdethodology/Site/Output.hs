module Mdethodology.Site.Output (applyTemplates, writeOutput) where

import Mdethodology.Types
import Mdethodology.Site.Render (renderBlocks)
import Mdethodology.Site.Template (renderTemplate)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import qualified Data.Map as Map

-- Templating now happens in writeOutput (which has the site's templates),
-- so this pass is a no-op kept for pipeline shape / future per-page work.
applyTemplates :: FilePath -> Pass
applyTemplates _ site = pure site

writeOutput :: FilePath -> Pass
writeOutput outDir site = do
  mapM_ writeOne (sitePages site)
  pure site
  where
    templates = siteTemplates site
    writeOne (Route r, page) = do
      let src   = pageSource page
          body  = renderBlocks (srcBody src)
          -- scope = every frontmatter scalar + the rendered body as {{content}}
          scope = Map.fromList (frontmatterVars (srcFrontmatter src) ++ [("content", body)])
          -- pick the page's template by name; fall back to a builtin if absent
          tmpl  = Map.findWithDefault defaultTemplate (pageTemplate page) templates
          file  = outDir </> pathFor r
      case renderTemplate scope tmpl of
        Right html -> do
          createDirectoryIfMissing True (takeDirectory file)
          writeFile file html
        Left err   -> ioError (userError (show err))
    -- pretty URLs: each route becomes a directory with an index.html,
    -- so a server resolves "/about" -> out/about/index.html automatically.
    pathFor "/"          = "index.html"
    pathFor ('/':rest)   = rest </> "index.html"
    pathFor s            = s </> "index.html"
    -- used only when a page names a layout that has no templates/<name>.html
    defaultTemplate =
      "<!doctype html><html><head><title>{{title}}</title></head>\
      \<body>{{content}}</body></html>"

-- Flatten a frontmatter map into string template variables.
frontmatterVars :: YamlValue -> [(String, String)]
frontmatterVars (YMap m) = [ (k, scalarText v) | (k, v) <- Map.toList m ]
frontmatterVars _        = []

scalarText :: YamlValue -> String
scalarText (YString s) = s
scalarText (YNumber n) = show n
scalarText (YBool b)   = if b then "true" else "false"
scalarText _           = ""
