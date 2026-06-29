module Mdethodology.Parser.Combinator where

import Control.Applicative (Alternative(..))

data Pos = Pos { posLine :: Int, posCol :: Int }
  deriving (Eq, Show)

data ParseError = ParseError Pos String
  deriving (Eq, Show)

-- A parser takes the current position and the remaining input,
-- and yields either an error, or (a result, new position, leftover input).
newtype Parser a = Parser
  { runParser :: Pos -> String -> Either ParseError (a, Pos, String) }


instance Functor Parser where
  fmap f (Parser p) = Parser $ \pos s ->
    case p pos s of
      Left  e               -> Left e            -- failure passes through untouched
      Right (a, pos', rest) -> Right (f a, pos', rest)  -- apply f to the result
