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
