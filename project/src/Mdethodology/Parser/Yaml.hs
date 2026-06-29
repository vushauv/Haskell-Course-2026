{-# LANGUAGE LambdaCase #-}

module Mdethodology.Parser.Yaml (yamlValue, parseYaml) where

import Control.Applicative
import Mdethodology.Parser.Combinator
import Mdethodology.Types
import qualified Data.Map as Map

-- A scalar is a bool, null, number, or bare string — tried in that order.
scalar :: Parser YamlValue
scalar = yBool <|> yNull <|> yNumber <|> yString
  where
    yBool   = (YBool True  <$ string "true")
          <|> (YBool False <$ string "false")
    yNull   = YNull <$ string "null"
    yNumber = YNumber . read <$> some (satisfy (`elem` "0123456789.-"))
    yString = YString <$> some (satisfy (`notElem` "\n:#"))


-- A block list: lines beginning with "- ".
yList :: Parser YamlValue
yList = YList <$> some item
  where item = string "- " *> yamlValue <* char '\n'

-- A block map: lines of "key: value".
yMap :: Parser YamlValue
yMap = (YMap . Map.fromList) <$> some pair
  where
    pair = do
      k <- some (satisfy (`notElem` ":\n"))
      _ <- string ": "
      v <- scalar
      _ <- char '\n'
      pure (k, v)

-- The entry point: a value is a list, or a map, or a scalar.
yamlValue :: Parser YamlValue
yamlValue = yList <|> yMap <|> scalar

parseYaml :: String -> Either ParseError YamlValue
parseYaml = parse yamlValue


