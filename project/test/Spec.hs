module Main (main) where

import Control.Applicative (many, some, (<|>))
import Data.Char (isDigit)
import Test.Hspec

import Mdethodology.Parser.Combinator

-- A small helper parser used across the tests.
digit :: Parser Char
digit = satisfy isDigit

main :: IO ()
main = hspec $ do
  describe "satisfy" $ do
    it "consumes a matching char" $
      parse (satisfy isDigit) "7" `shouldBe` Right '7'
    it "fails on a non-matching char (with location)" $
      parse (satisfy isDigit) "x"
        `shouldBe` Left (ParseError (Pos 1 1) "unexpected 'x'")
    it "fails on empty input" $
      parse (satisfy isDigit) ""
        `shouldBe` Left (ParseError (Pos 1 1) "unexpected end of input")

  describe "char" $ do
    it "matches the exact char" $
      parse (char 'a') "a" `shouldBe` Right 'a'
    it "rejects a different char" $
      parse (char 'a') "b"
        `shouldBe` Left (ParseError (Pos 1 1) "unexpected 'b'")

  describe "string" $ do
    it "matches a full literal" $
      parse (string "true") "true" `shouldBe` Right "true"
    it "fails at the first mismatching char" $
      parse (string "true") "trUe"
        `shouldBe` Left (ParseError (Pos 1 3) "unexpected 'U'")

  describe "spaces" $ do
    it "consumes spaces and tabs, succeeding with unit" $
      parse spaces "   \t " `shouldBe` Right ()
    it "succeeds consuming nothing when there is no whitespace" $
      parse spaces "abc" `shouldBe` Right ()

  describe "fmap (Functor)" $ do
    it "transforms a successful result" $
      parse (fmap (: []) (char 'z')) "z" `shouldBe` Right "z"

  describe "many / some (Alternative)" $ do
    it "many collects zero or more" $
      runParser (many digit) (Pos 1 1) "123abc"
        `shouldBe` Right ("123", Pos 1 4, "abc")
    it "many succeeds on no matches" $
      runParser (many digit) (Pos 1 1) "abc"
        `shouldBe` Right ("", Pos 1 1, "abc")
    it "some requires at least one" $
      parse (some digit) "x"
        `shouldBe` Left (ParseError (Pos 1 1) "unexpected 'x'")

  describe "<|> (choice)" $ do
    it "takes the left branch when it succeeds" $
      parse (char 'a' <|> char 'b') "a" `shouldBe` Right 'a'
    it "falls back to the right branch on failure" $
      parse (char 'a' <|> char 'b') "b" `shouldBe` Right 'b'
