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


instance Applicative Parser where
  pure x = Parser $ \pos s -> Right (x, pos, s)           -- succeed, consume nothing
  Parser pf <*> Parser px = Parser $ \pos s ->
    case pf pos s of
      Left e -> Left e
      Right (f, pos', rest) ->                            -- first parser gives a function
        case px pos' rest of
          Left e -> Left e
          Right (x, pos'', rest') -> Right (f x, pos'', rest')  -- feed it the second result


instance Monad Parser where
  Parser p >>= f = Parser $ \pos s ->
    case p pos s of
      Left  e               -> Left e
      Right (a, pos', rest) -> runParser (f a) pos' rest   -- f *chooses* the next parser using a


instance Alternative Parser where
  empty = Parser $ \pos _ -> Left (ParseError pos "empty")
  Parser p <|> Parser q = Parser $ \pos s ->
    case p pos s of
      Left _    -> q pos s     -- left failed: try the right on the *original* input
      success   -> success     -- left worked: keep it
