module Mdethodology.Site.Template (renderTemplate) where

import Control.Applicative
import Mdethodology.Parser.Combinator
import qualified Data.Map as Map

data Chunk = Lit String | Hole String

template :: Parser [Chunk]
template = many (hole <|> lit)
  where
    hole = do _ <- string "{{"; spaces
              name <- some (satisfy (`notElem` " }"))
              spaces; _ <- string "}}"
              pure (Hole name)
    lit  = Lit <$> some (satisfy (/= '{'))

renderTemplate :: Map.Map String String -> String -> Either ParseError String
renderTemplate scope src = do
  chunks <- parse template src
  pure (concatMap fill chunks)
  where
    fill (Lit s)    = s
    fill (Hole name)= Map.findWithDefault "" name scope
