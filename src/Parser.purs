module Parser where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Lazy (class Lazy, defer)
import Data.Array (cons, reverse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (uncons)
import Data.Tuple (Tuple(..), snd)
import Data.Unfoldable (class Unfoldable, none)

type ParserState a = Tuple String a

type ParseFunction e a = 
  ParserError e => String -> Either e (ParserState a)

class ParserError e where 
  eof :: e
  invalidChar :: Char -> e
  parseFail :: String -> e

newtype Parser e a = Parser (ParseFunction e a)

instance functorParser :: Functor (Parser e) where
  map f g = Parser \s -> map f <$> parse g s

instance applyParser :: Apply (Parser e) where 
  apply f g = Parser \s -> case parse f s of
    Left err -> Left err
    Right (Tuple s1 h) -> case parse g s1 of
      Left err -> Left err
      Right (Tuple s2 x) -> Right $ Tuple s2 (h x)

instance applicativeParser :: Applicative (Parser e) where
  pure x = Parser \s -> pure $ Tuple s x

instance altParser :: Alt (Parser e) where
  alt l r = Parser \s -> case parse l s of
    Left _ -> parse r s
    Right x -> Right x

instance bindParser :: Bind (Parser e) where
  bind p f = Parser \s -> do
    Tuple s1 x <- parse p s
    parse (f x) s1

instance lazyParser :: Lazy (Parser e a) where
  defer f = Parser \s -> parse (f unit) s

parse :: ∀ e a. Parser e a -> ParseFunction e a
parse (Parser f) = f

unwrap :: ∀ e a. Either e (ParserState a) -> Either e a 
unwrap = map snd

char :: ∀ e. Parser e Char
char = Parser \s -> case uncons s of 
  Nothing -> Left eof
  Just {head, tail} -> Right $ Tuple tail head 

fail :: ∀ e a. ParserError e => e -> Parser e a
fail e = Parser \_ -> Left e

invalid :: ∀ e a. ParserError e => Char -> Parser e a
invalid c = Parser \_ -> Left $ invalidChar c


failWith :: ∀ e a. ParserError e => String -> Parser e a
failWith s = Parser \_ -> Left $ parseFail s

satisfy :: ∀ e. ParserError e => String -> (Char -> Boolean) -> Parser e Char
satisfy expected pred = char
  >>= \c -> if pred c then pure c else failWith expected

satisfy' :: ∀ e. ParserError e => (Char -> Boolean) -> Parser e Char
satisfy' pred = char 
  >>= \c -> if pred c then pure c else invalid c

many :: ∀ e a f. Unfoldable f => (a -> f a -> f a) -> Parser e a -> Parser e (f a)
many cons p = some cons p <|> pure none

many' :: ∀ e a. Parser e a -> Parser e (Array a)
many' = many cons

some :: ∀ e a f. Unfoldable f => (a -> f a -> f a) -> Parser e a -> Parser e (f a)
some cons p = cons <$> p <*> defer \_ -> many cons p

some' :: ∀ e a. Parser e a -> Parser e (Array a)
some' = some cons

parseUntil :: ∀ e a f. 
  Unfoldable f => 
  ParserError e => Eq e => -- error gotta have Eq so we can find eof
  (a -> f a -> f a) -> -- cons function for Foldable f 
  (f a -> f a) ->  -- reverse function for Foldable f
  Parser e a -> 
  Parser e (f a)
parseUntil cons rev p = Parser (aux none)
  where
    aux acc s = 
      case (parse p) s of 
        Left e -> if e == eof then pure $ Tuple s (rev acc) else Left e
        Right (Tuple s2 x) -> aux (cons x acc) s2 

parseUntil' :: ∀ e a. ParserError e => Eq e => Parser e a -> Parser e (Array a)
parseUntil' = parseUntil cons reverse

instance stringParserError :: ParserError String where
  eof = "End of file."
  parseFail s = "Parsing failure: " <> s  <> "."
  invalidChar c = "'" <> show c <> "'" <> " is an invalid character."