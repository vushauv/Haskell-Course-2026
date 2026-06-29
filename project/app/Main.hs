module Main (main) where

import Mdethodology.Site.Pipeline (runPipeline, defaultPipeline)
import Mdethodology.Types (Site(..), YamlValue(..))
import System.Environment (getArgs)
import qualified Data.Map as Map

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("build" : src : "-o" : out : _) -> do
      _ <- runPipeline (defaultPipeline src out) (Site [] YNull Map.empty Map.empty)
      putStrLn ("built " ++ src ++ " -> " ++ out)
    _ -> putStrLn "usage: mdethodology build <src> -o <out>"
