module Mdethodology.Site.Pipeline (Pass, runPipeline, defaultPipeline) where

import Mdethodology.Types
import Mdethodology.Site.Loader (loadSources)
import Mdethodology.Site.Links  (resolveLinks)
import Mdethodology.Site.Output (writeOutput, applyTemplates)
import Control.Monad (foldM)

-- Run passes left-to-right, feeding each one the previous result.
runPipeline :: [Pass] -> Site -> IO Site
runPipeline passes site0 = foldM (\site pass -> pass site) site0 passes

defaultPipeline :: FilePath -> FilePath -> [Pass]
defaultPipeline src out =
  [ loadSources src
  , resolveLinks
  , applyTemplates src
  , writeOutput out
  ]
