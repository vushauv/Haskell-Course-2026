module Mdethodology.Site.Loader (loadSources) where

import Mdethodology.Types
import Mdethodology.Site.Routes (routeFor)
import Mdethodology.Parser.Yaml (parseYaml)
import Mdethodology.Parser.Markdown (parseMarkdown)
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import Control.Monad (filterM, forM)

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
  pure (Site pages YNull)
  where
    templateOf _ = "default"     -- (read `layout:` from frontmatter as an exercise)
