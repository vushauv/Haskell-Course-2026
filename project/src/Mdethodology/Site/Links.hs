module Mdethodology.Site.Links (resolveLinks) where

import Mdethodology.Types
import Data.List (find)

-- Check that an internal target matches a known page; otherwise Left an error.
checkTarget :: [Route] -> LinkTarget -> Either String LinkTarget
checkTarget _      ext@(External _) = Right ext
checkTarget routes (Internal t)     =
  let wanted = Route (normalise t)
  in if wanted `elem` routes
       then Right (Internal (unRoute wanted))
       else Left ("dangling internal link: " ++ t)
  where normalise = id   -- map "./about.md" -> "/about"; left as an exercise

resolveSite :: Site -> Either String Site
resolveSite site = do
  let routes = map fst (sitePages site)
  pages' <- traverse (resolvePage routes) (sitePages site)
  pure site { sitePages = pages' }

resolvePage :: [Route] -> (Route, Page) -> Either String (Route, Page)
resolvePage routes (r, page) = do
  -- walk the body, checking every link; any Left aborts the whole build
  mapM_ (checkBlock routes) (srcBody (pageSource page))
  pure (r, page)

checkBlock :: [Route] -> Block -> Either String ()
checkBlock routes b = case b of
  Paragraph ins -> mapM_ (checkInline routes) ins
  Heading _ ins -> mapM_ (checkInline routes) ins
  _             -> Right ()

checkInline :: [Route] -> Inline -> Either String ()
checkInline routes (Link _ tgt) = () <$ checkTarget routes tgt
checkInline _      _            = Right ()

resolveLinks :: Pass
resolveLinks site = case resolveSite site of
  Right s  -> pure s
  Left err -> ioError (userError err)   -- abort the build with a clear message
