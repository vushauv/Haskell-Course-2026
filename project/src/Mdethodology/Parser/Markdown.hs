module Mdethodology.Parser.Markdown where

import Control.Applicative
import Mdethodology.Parser.Combinator
import Mdethodology.Types
import Data.List (isPrefixOf)

inline :: Parser Inline
inline = strong <|> emph <|> code <|> link <|> text
  where
    strong = Strong <$> between (string "**") (string "**") (some inlineNoStar)
    emph   = Emph   <$> between (char   '*' ) (char   '*' ) (some inlineNoStar)
    code   = Code   <$> between (char   '`' ) (char   '`' ) (some (satisfy (/= '`')))
    text   = Text   <$> some (satisfy (`notElem` "*`[\n"))

    -- inside emphasis we disallow more '*' so the closing delimiter wins
    inlineNoStar = code <|> link <|> (Text <$> some (satisfy (`notElem` "*`[\n")))

-- [visible text](target)
link :: Parser Inline
link = do
  _      <- char '['
  label  <- many (satisfy (/= ']'))
  _      <- string "]("
  target <- many (satisfy (/= ')'))
  _      <- char ')'
  pure (Link [Text label] (classify target))
  where
    classify t
      | "http" `isPrefixOf` t = External t -- TODO: in the future another way of defining external/internal
      | otherwise             = Internal t

-- helper: run `open`, then `p`, then `close`, keeping only p's result
between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close


block :: Parser Block
block = heading <|> codeBlock <|> bulletList <|> paragraph
  where
    heading = do
      hashes <- some (char '#')
      _      <- char ' '
      ins    <- some inline
      _      <- char '\n'
      pure (Heading (length hashes) ins)

    paragraph = do
      ins <- some inline
      _   <- char '\n'
      pure (Paragraph ins)

    bulletList = BulletList <$> some item
      where item = do
              _   <- string "- "
              ins <- some inline
              _   <- char '\n'
              pure [Paragraph ins]

    codeBlock = do
      _    <- string "```"
      lang <- many (satisfy (/= '\n'));  _ <- char '\n'
      body <- manyTill anyChar (string "```")
      pure (CodeBlock (if null lang then Nothing else Just lang) body)

-- A document is many blocks separated by blank lines.
document :: Parser [Block]
document = many (block <* many (char '\n'))

parseMarkdown :: String -> Either ParseError [Block]
parseMarkdown = parse document
