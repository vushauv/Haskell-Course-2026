module Mdethodology.Site.Loader (loadSources) where

import Mdethodology.Types
import Mdethodology.Site.Routes (routeFor)
import Mdethodology.Parser.Yaml (parseYaml)
import Mdethodology.Parser.Markdown (parseMarkdown)
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, takeBaseName, takeDirectory, splitDirectories)
import Control.Monad (forM)
import qualified Data.Map as Map

-- Recursively list every file under a directory.
walk :: FilePath -> IO [FilePath]
walk dir = do
  entries <- listDirectory dir
  paths   <- forM entries $ \e -> do
    let full = dir </> e
    isDir <- doesDirectoryExist full
    if isDir then walk full else pure [full]
  pure (concat paths)

  -- Split a .md file into its YAML frontmatter and Markdown body.
splitFrontmatter :: String -> (String, String)
splitFrontmatter content =
  case lines content of
    ("---" : rest) ->
      let (fm, body) = break (== "---") rest
      in (unlines fm, unlines (drop 1 body))
    _ -> ("", content)

-- Load every templates/<name>.html under the root into a name -> contents map.
loadTemplates :: FilePath -> IO (Map.Map String String)
loadTemplates root = do
  files <- walk root
  let tmplFiles =
        [ f | f <- files
            , takeExtension f == ".html"
            , "templates" `elem` splitDirectories (takeDirectory f) ]
  pairs <- forM tmplFiles $ \f -> do
    contents <- readFile f
    pure (takeBaseName f, contents)   -- "default.html" -> "default"
  pure (Map.fromList pairs)

loadSources :: FilePath -> Pass
loadSources root _ = do            -- ignore the incoming (empty) Site; we build it
  files <- walk root
  let mdFiles = filter ((== ".md") . takeExtension) files
  pages <- forM mdFiles $ \path -> do
    raw <- readFile path
    let (fmText, bodyText) = splitFrontmatter raw
    case (parseYaml fmText, parseMarkdown bodyText) of
      (Right fm, Right body) ->
        let src   = SourceFile path fm body
            route = routeFor root path
        in pure (route, Page src route (templateOf fm))
      (Left e, _) -> ioError (userError ("YAML error: "  ++ show e))
      (_, Left e) -> ioError (userError ("Markdown error: " ++ show e))
  templates <- loadTemplates root
  pure (Site pages YNull templates)
  where
    -- the page's template name comes from `layout:` in frontmatter (default "default")
    templateOf (YMap m) = case Map.lookup "layout" m of
                            Just (YString t) -> t
                            _                -> "default"
    templateOf _        = "default"
