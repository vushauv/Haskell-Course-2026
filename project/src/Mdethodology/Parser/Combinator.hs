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


-- implementation of typeclasses

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


-- Primitive parsers

-- Advance the position, handling newlines.
advance :: Pos -> Char -> Pos
advance (Pos l _) '\n' = Pos (l + 1) 1
advance (Pos l c) _    = Pos l (c + 1)


-- Consume one char if it satisfies the predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy ok = Parser $ \pos s ->
  case s of
    (c:cs) | ok c      -> Right (c, advance pos c, cs)
           | otherwise -> Left (ParseError pos ("unexpected '" ++ [c] ++ "'"))
    []                 -> Left (ParseError pos "unexpected end of input")

char :: Char -> Parser Char
char c = satisfy (== c)


-- `string` parses each char in turn. `traverse` (free from Applicative!) turns
-- a list of parsers into a parser of a list.
string :: String -> Parser String
string = traverse char

spaces :: Parser ()
spaces = () <$ many (satisfy (`elem` " \t"))

anyChar :: Parser Char
anyChar = satisfy (const True)

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = go
  where
    go =  (end *> pure [])          -- end matched: stop, return []
      <|> ((:) <$> p <*> go)        -- else: parse one p, then recurse


-- Succeed only at the end of input; otherwise fail with the offending location.
eof :: Parser ()
eof = Parser $ \pos s ->
  case s of
    []    -> Right ((), pos, s)
    (c:_) -> Left (ParseError pos ("unexpected '" ++ [c] ++ "', expected end of input"))

-- The top-level entry point: run a parser on a whole string.
parse :: Parser a -> String -> Either ParseError a
parse p s =
  case runParser p (Pos 1 1) s of
    Left  e          -> Left e
    Right (a, _, _)  -> Right a


