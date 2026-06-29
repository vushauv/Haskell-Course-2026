module Main (main) where

import Control.Applicative (many, some, (<|>))
import Data.Char (isDigit)
import qualified Data.Map as Map
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>), takeFileName)
import Test.Hspec
import Test.QuickCheck

import Mdethodology.Parser.Combinator
import Mdethodology.Parser.Yaml (parseYaml)
import Mdethodology.Parser.Markdown (parseMarkdown)
import Mdethodology.Site.Routes (routeFor, routeForPage)
import Mdethodology.Site.Render (renderBlocks)
import Mdethodology.Site.Template (renderTemplate)
import Mdethodology.Site.Links (resolveLinks)
import Mdethodology.Site.Loader (loadSources)
import Mdethodology.Site.Pipeline (runPipeline, defaultPipeline)
import Mdethodology.Types

-- A small helper parser used across the combinator tests.
digit :: Parser Char
digit = satisfy isDigit

main :: IO ()
main = hspec $ do
  combinatorSpec
  yamlSpec
  markdownSpec
  routesSpec
  templateSpec
  renderSpec
  linksSpec
  propertySpec
  endToEndSpec

--------------------------------------------------------------------------------
-- 1. Parser combinator primitives
--------------------------------------------------------------------------------

combinatorSpec :: Spec
combinatorSpec = do
  describe "satisfy" $ do
    it "consumes a matching char" $
      parse (satisfy isDigit) "7" `shouldBe` Right '7'
    it "fails on a non-matching char (with location)" $
      parse (satisfy isDigit) "x"
        `shouldBe` Left (ParseError (Pos 1 1) "unexpected 'x'")
    it "fails on empty input" $
      parse (satisfy isDigit) ""
        `shouldBe` Left (ParseError (Pos 1 1) "unexpected end of input")

  describe "char / string" $ do
    it "char matches the exact char" $
      parse (char 'a') "a" `shouldBe` Right 'a'
    it "string matches a full literal" $
      parse (string "true") "true" `shouldBe` Right "true"
    it "string fails at the first mismatching char" $
      parse (string "true") "trUe"
        `shouldBe` Left (ParseError (Pos 1 3) "unexpected 'U'")

  describe "many / some / <|>" $ do
    it "many collects zero or more, tracking position" $
      runParser (many digit) (Pos 1 1) "123abc"
        `shouldBe` Right ("123", Pos 1 4, "abc")
    it "many succeeds on no matches" $
      runParser (many digit) (Pos 1 1) "abc"
        `shouldBe` Right ("", Pos 1 1, "abc")
    it "some requires at least one" $
      parse (some digit) "x"
        `shouldBe` Left (ParseError (Pos 1 1) "unexpected 'x'")
    it "<|> falls back to the right branch on failure" $
      parse (char 'a' <|> char 'b') "b" `shouldBe` Right 'b'

--------------------------------------------------------------------------------
-- 2. YAML parser
--------------------------------------------------------------------------------

yamlSpec :: Spec
yamlSpec = describe "YAML parser" $ do
  it "parses true / false / null" $ do
    parseYaml "true\n"  `shouldBe` Right (YBool True)
    parseYaml "false\n" `shouldBe` Right (YBool False)
    parseYaml "null\n"  `shouldBe` Right YNull
  it "parses a number" $
    parseYaml "42\n" `shouldBe` Right (YNumber 42)
  it "parses a bare string" $
    parseYaml "hello\n" `shouldBe` Right (YString "hello")
  it "parses a map of scalars" $
    parseYaml "title: Home\nlayout: default\n"
      `shouldBe` Right (YMap (Map.fromList
                         [ ("title",  YString "Home")
                         , ("layout", YString "default") ]))
  it "parses a list of scalars" $
    parseYaml "- a\n- b\n"
      `shouldBe` Right (YList [YString "a", YString "b"])

--------------------------------------------------------------------------------
-- 3. Markdown parser
--------------------------------------------------------------------------------

markdownSpec :: Spec
markdownSpec = describe "Markdown parser" $ do
  it "parses a heading with its level" $
    parseMarkdown "## Hi\n" `shouldBe` Right [Heading 2 [Text "Hi"]]
  it "parses a paragraph" $
    parseMarkdown "hello world\n" `shouldBe` Right [Paragraph [Text "hello world"]]
  it "parses strong, emphasis and inline code mixed in a paragraph" $
    parseMarkdown "This is **bold** and *it* and `c`\n"
      `shouldBe` Right [Paragraph
        [ Text "This is ", Strong [Text "bold"]
        , Text " and ", Emph [Text "it"]
        , Text " and ", Code "c" ]]
  it "classifies an external link" $
    parseMarkdown "[a](https://x.com)\n"
      `shouldBe` Right [Paragraph [Link [Text "a"] (External "https://x.com")]]
  it "classifies an internal link" $
    parseMarkdown "[a](/about)\n"
      `shouldBe` Right [Paragraph [Link [Text "a"] (Internal "/about")]]
  it "parses a bullet list" $
    parseMarkdown "- one\n- two\n"
      `shouldBe` Right [BulletList [ [Paragraph [Text "one"]]
                                   , [Paragraph [Text "two"]] ]]
  it "parses a fenced code block with a language" $
    parseMarkdown "```haskell\nx = 1\n```\n"
      `shouldBe` Right [CodeBlock (Just "haskell") "x = 1\n"]

--------------------------------------------------------------------------------
-- 4. Route derivation (pure table)
--------------------------------------------------------------------------------

routesSpec :: Spec
routesSpec = describe "route derivation" $ do
  it "maps index.md to /" $
    routeFor "site" "site/index.md" `shouldBe` Route "/"
  it "drops the .md extension" $
    routeFor "site" "site/about.md" `shouldBe` Route "/about"
  it "keeps nested paths" $
    routeFor "site" "site/blog/intro.md" `shouldBe` Route "/blog/intro"
  it "uses an explicit slug from frontmatter" $
    routeForPage "site" "site/whatever.md"
      (YMap (Map.fromList [("slug", YString "/custom")])) `shouldBe` Route "/custom"
  it "adds a leading slash to a bare slug" $
    routeForPage "site" "site/whatever.md"
      (YMap (Map.fromList [("slug", YString "custom")])) `shouldBe` Route "/custom"
  it "falls back to the path when there is no slug" $
    routeForPage "site" "site/about.md" YNull `shouldBe` Route "/about"

--------------------------------------------------------------------------------
-- 5. Template substitution (fixed table)
--------------------------------------------------------------------------------

templateSpec :: Spec
templateSpec = describe "template substitution" $ do
  it "fills holes from the scope" $
    renderTemplate (Map.fromList [("title", "Hi"), ("content", "B")])
                   "<h1>{{ title }}</h1>{{ content }}"
      `shouldBe` Right "<h1>Hi</h1>B"
  it "renders a missing variable as empty" $
    renderTemplate Map.empty "x{{ y }}z" `shouldBe` Right "xz"
  it "passes literal text through unchanged" $
    renderTemplate Map.empty "plain text" `shouldBe` Right "plain text"

--------------------------------------------------------------------------------
-- 6. Rendering AST -> HTML
--------------------------------------------------------------------------------

renderSpec :: Spec
renderSpec = describe "render" $ do
  it "renders a heading" $
    renderBlocks [Heading 1 [Text "Hi"]] `shouldBe` "<h1>Hi</h1>"
  it "renders nested inline emphasis" $
    renderBlocks [Paragraph [Strong [Text "b"], Text " ", Emph [Text "i"]]]
      `shouldBe` "<p><strong>b</strong> <em>i</em></p>"
  it "renders a link with its href" $
    renderBlocks [Paragraph [Link [Text "h"] (Internal "/about")]]
      `shouldBe` "<p><a href=\"/about\">h</a></p>"
  it "escapes HTML special characters in text" $
    renderBlocks [Paragraph [Text "a<b>&c"]]
      `shouldBe` "<p>a&lt;b&gt;&amp;c</p>"

--------------------------------------------------------------------------------
-- 7. Link resolution
--------------------------------------------------------------------------------

mkPage :: String -> [Block] -> (Route, Page)
mkPage r blocks =
  let src = SourceFile ("/src" ++ r) YNull blocks
  in (Route r, Page src (Route r) "default")

linksSpec :: Spec
linksSpec = describe "link resolution" $ do
  it "accepts a site whose internal links all resolve" $ do
    let site = Site [ mkPage "/"      [Paragraph [Link [Text "a"] (Internal "/about")]]
                    , mkPage "/about" [] ]
                    YNull Map.empty
    resolveLinks site `shouldReturn` site
  it "rejects a dangling internal link" $ do
    let site = Site [ mkPage "/" [Paragraph [Link [Text "a"] (Internal "/missing")]] ]
                    YNull Map.empty
    resolveLinks site `shouldThrow` anyException

--------------------------------------------------------------------------------
-- 8. Property: YAML scalar round-trip (serialise then parse = id)
--------------------------------------------------------------------------------

genScalar :: Gen YamlValue
genScalar = oneof
  [ YBool <$> arbitrary
  , YNumber . fromIntegral <$> (choose (-9999, 9999) :: Gen Int)
  , pure YNull
  ]

serialiseScalar :: YamlValue -> String
serialiseScalar (YBool True)  = "true"
serialiseScalar (YBool False) = "false"
serialiseScalar YNull         = "null"
serialiseScalar (YNumber n)   = show n
serialiseScalar _             = ""

propertySpec :: Spec
propertySpec = describe "properties" $
  it "round-trips YAML scalars: parse (serialise v) == v" $
    property $ forAll genScalar $ \v ->
      parseYaml (serialiseScalar v ++ "\n") === Right v

--------------------------------------------------------------------------------
-- 9. End-to-end builds against fixtures
--------------------------------------------------------------------------------

buildFixture :: FilePath -> IO FilePath
buildFixture src = do
  tmp <- getTemporaryDirectory
  let out = tmp </> "mdethodology-test" </> takeFileName src
  removePathForcibly out
  _ <- runPipeline (defaultPipeline src out) (Site [] YNull Map.empty)
  pure out

endToEndSpec :: Spec
endToEndSpec = describe "end-to-end build" $ do
  it "loads config.yml into siteConfig" $ do
    site <- loadSources "test/fixtures/basic" (Site [] YNull Map.empty)
    siteConfig site
      `shouldBe` YMap (Map.fromList [("siteName", YString "mdethodology")])
  it "builds the basic fixture with templated, routed pages" $ do
    out <- buildFixture "test/fixtures/basic"
    idx <- readFile (out </> "index.html")
    idx `shouldContain` "<title>Home</title>"
    idx `shouldContain` "<h1>Welcome</h1>"
    idx `shouldContain` "<a href=\"/about\">about</a>"
    abt <- readFile (out </> "about" </> "index.html")
    abt `shouldContain` "<title>About</title>"
    removePathForcibly out
  it "fails the build on a dangling internal link" $
    buildFixture "test/fixtures/broken" `shouldThrow` anyException
