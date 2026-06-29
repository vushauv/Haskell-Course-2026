module Mdethodology.Types where

import Data.Map (Map)

data YamlValue
   = YString String
   | YNumber Double
   | YBool   Bool
   | YNull   
   | YList   [YamlValue]
   | YMap    (Map String YamlValue)
   deriving (Eq, Show)


data Block
  = Heading Int [Inline]            -- '#' level + the heading text
  | Paragraph [Inline]
  | CodeBlock (Maybe String) String -- optional language tag + raw code
  | BulletList [[Block]]            -- each item is itself a list of blocks
  deriving (Eq, Show)


data Inline
  = Text   String
  | Emph   [Inline]                 -- *italic*
  | Strong [Inline]                 -- **bold**
  | Code   String                   -- `inline code`
  | Link   [Inline] LinkTarget      -- link text + where it points
  deriving (Eq, Show)


data LinkTarget
  = External String                 -- http://...  — leave alone
  | Internal String                 -- ./about.md  — resolve later
  deriving (Eq, Show)


data SourceFile = SourceFile
  { srcPath        :: FilePath
  , srcFrontmatter :: YamlValue
  , srcBody        :: [Block]
  } deriving (Eq, Show)


newtype Route = Route { unRoute :: String }
  deriving (Eq, Ord, Show)


data Page = Page
  { pageSource   :: SourceFile
  , pageRoute    :: Route
  , pageTemplate :: String
  } deriving (Eq, Show)


data Site = Site
  { sitePages  :: [(Route, Page)]
  , siteConfig :: YamlValue
  } deriving (Eq, Show)

-- A build step: take the site, do some IO, return the updated site.
type Pass = Site -> IO Site
