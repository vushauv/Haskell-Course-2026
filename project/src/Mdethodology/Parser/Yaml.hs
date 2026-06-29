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

-- Count the leading spaces of the current line (the line's indentation).
indent :: Parser Int
indent = length <$> many (char ' ')

-- A scalar that fills the rest of the line.
scalarLine :: Parser YamlValue
scalarLine = scalar <* char '\n'

-- The value following "key:" — either an inline scalar on the same line, or a
-- nested block (map/list) on the following lines, indented deeper than `parent`.
valueFor :: Int -> Parser YamlValue
valueFor parent =
      (char ' ' *> scalarLine)
  <|> (char '\n' *> block (parent + 1))

-- A block value: a map or a list, whose entries are indented per the predicate.
block :: Int -> Parser YamlValue
block minI = mapBlock minI <|> listBlock minI

-- A block map: a first entry whose indent is >= minI fixes the level, then any
-- number of sibling entries at exactly that indent.
mapBlock :: Int -> Parser YamlValue
mapBlock minI = do
  (i, p) <- mapEntry (>= minI)
  ps     <- many (mapEntry (== i))
  pure (YMap (Map.fromList (p : map snd ps)))

mapEntry :: (Int -> Bool) -> Parser (Int, (String, YamlValue))
mapEntry ok = do
  i <- indent
  if ok i then pure () else empty
  k <- some (satisfy (`notElem` ":\n"))
  _ <- char ':'
  v <- valueFor i
  pure (i, (k, v))

-- A block list: lines of "- scalar", all at the same indent.
listBlock :: Int -> Parser YamlValue
listBlock minI = do
  (i, x) <- listItem (>= minI)
  xs     <- many (listItem (== i))
  pure (YList (x : map snd xs))

listItem :: (Int -> Bool) -> Parser (Int, YamlValue)
listItem ok = do
  i <- indent
  if ok i then pure () else empty
  _ <- string "- "
  v <- scalar
  _ <- char '\n'
  pure (i, v)

-- The entry point: a block (map/list), or a single scalar line/value.
yamlValue :: Parser YamlValue
yamlValue = block 0 <|> scalarLine <|> scalar

-- Require the whole input to be consumed (modulo trailing blank space), so
-- malformed YAML is reported with a location instead of silently truncated.
parseYaml :: String -> Either ParseError YamlValue
parseYaml = parse (yamlValue <* many (satisfy (`elem` " \n")) <* eof)
