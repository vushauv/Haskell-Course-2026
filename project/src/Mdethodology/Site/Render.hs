module Mdethodology.Site.Render (renderBlocks) where

import Mdethodology.Types

renderBlocks :: [Block] -> String
renderBlocks = concatMap renderBlock

renderBlock :: Block -> String
renderBlock (Heading n ins)   = tag ("h" ++ show n) (renderInlines ins)
renderBlock (Paragraph ins)   = tag "p" (renderInlines ins)
renderBlock (BulletList items)= tag "ul" (concatMap (tag "li" . renderBlocks) items)
renderBlock (CodeBlock _ body)= tag "pre" (tag "code" (escape body))

renderInlines :: [Inline] -> String
renderInlines = concatMap renderInline

renderInline :: Inline -> String
renderInline (Text s)        = escape s
renderInline (Emph ins)      = tag "em"     (renderInlines ins)
renderInline (Strong ins)    = tag "strong" (renderInlines ins)
renderInline (Code s)        = tag "code"   (escape s)
renderInline (Link ins tgt)  = "<a href=\"" ++ href tgt ++ "\">"
                                 ++ renderInlines ins ++ "</a>"
  where href (External u) = u
        href (Internal r) = r

tag :: String -> String -> String
tag t inner = "<" ++ t ++ ">" ++ inner ++ "</" ++ t ++ ">"

escape :: String -> String
escape = concatMap esc
  where esc '<' = "<"; esc '>' = ">"; esc '&' = "&"; esc c = [c]
