module Equation.Textual where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Array (cons, foldl)
import Data.Array (reverse, uncons) as A
import Data.Bifunctor (lmap)
import Data.CodePoint.Unicode (isDecDigit)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (CodePoint, Pattern(..), codePointFromChar, fromCodePointArray, indexOf, splitAt, toCodePointArray)
import Data.String.CodeUnits (fromCharArray, singleton, uncons)
import Data.Tuple (Tuple(..), fst, snd)
import Equation.Expression (SymbolMap, parseSymbols')
import Parser (class ParserError, Parser(..), many', some', parse, parseFail, satisfy')

newtype TextEquation = TextEquation {
  main :: String,
  above :: Maybe TextEquation,
  below :: Maybe TextEquation,
  right :: Maybe TextEquation
}

debugShow :: TextEquation -> String
debugShow (TextEquation t) = main <> above <> below <> right
  where
    above = fromMaybe "" (("^" <> _) <$> show <$> t.above)
    below = fromMaybe "" (("/" <> _) <$> show <$> t.below)
    right = fromMaybe "" (map show t.right)
    main = if t.main /= "" then "{" <> t.main <> "}" else ""

instance showTextEquation :: Show TextEquation where
  show (TextEquation t) = t.main <> above <> below <> right
    where
      above = fromMaybe "" (("^" <> _) <$> show <$> t.above)
      below = fromMaybe "" (("/" <> _) <$> show <$> t.below)
      right = fromMaybe "" (map show t.right)

emptyTextEquation :: TextEquation 
emptyTextEquation = TextEquation {main: "", above: Nothing, below: Nothing, right: Nothing}

tail' :: String -> String
tail' s = fromMaybe "" $ _.tail <$> uncons s

--cuts out the splitting character
splitAt' :: Int -> String -> {before :: String, after :: String}
splitAt' a s = {before, after: after'}
  where
    {before, after} = splitAt a s
    after' = tail' after

reverse :: String -> String 
reverse = toCodePointArray >>> A.reverse >>> fromCodePointArray

isParenthesis :: Char -> Boolean 
isParenthesis c = 
  char == lp || char == rp
  where
    char = codePointFromChar c
    lp = codePointFromChar ')'
    rp = codePointFromChar '('

parseParenthesisLevel :: ∀ e. Parser e Int
parseParenthesisLevel = Parser \s -> do
  (Tuple l ps) <- (parse $ many' $ satisfy' isParenthesis) s
  Right $ Tuple l  (aux ps)
  where 
  aux = foldl (\acc e -> if e == '(' then acc + 1 else acc - 1) 0

isPeriod :: CodePoint -> Boolean
isPeriod c = c == codePointFromChar '.' 

isDigit :: Char -> Boolean 
isDigit = codePointFromChar >>> (isDecDigit || isPeriod)

digits :: ∀ e. ParserError e => Parser e String 
digits = fromCharArray <$> (some' $ satisfy' isDigit)

simplePhrase :: ∀ e. ParserError e => SymbolMap -> Parser e String 
simplePhrase sm = Parser \s -> do
  symbols <- lmap parseFail (parseSymbols' sm s)  
  cut <- note (parseFail "This error shouldn't be possible: simplePhrase") $ A.uncons symbols
  pure $ Tuple (foldl (<>) "" cut.tail) cut.head

parenthesisPhrase :: ∀ e. ParserError e => Int -> Parser e String 
parenthesisPhrase parenthesis = Parser \s -> do
  aux "" s parenthesis
  where 
    aux acc str 0 = pure $ Tuple str acc
    aux acc str par = 
      case uncons str of 
        Nothing -> pure $ Tuple "" acc 
        Just {head: ')', tail} -> aux (acc <> ")") tail (par + 1)
        Just {head: '(', tail} -> aux (acc <> "(") tail (par - 1)
        Just {head, tail} -> aux (acc <> singleton head) tail par

phrase :: ∀ e. ParserError e => Int -> SymbolMap -> Parser e String 
phrase par sm =
  if par == 0 then 
    digits <|> simplePhrase sm 
  else 
    parenthesisPhrase par

type PartPart = {
  before :: String,
  left :: String, 
  right :: String
}

repeat :: Int -> Char -> String 
repeat 0 _ = ""
repeat times char = fromCharArray $ aux [] times
  where
    aux :: Array Char -> Int -> Array Char
    aux acc 0 = acc
    aux acc count = 
      aux (cons char acc) (count - 1)

part :: ∀ e. ParserError e => Char -> SymbolMap ->  Parser e PartPart
part char sm = Parser \s -> do
  expi <- note (parseFail "This error shouldn't propagate: part") $ indexOf (Pattern $ singleton char) s
  let split = splitAt' expi s 
  Tuple before parb <- parse parseParenthesisLevel $ reverse split.before
  left <- (parse $ phrase (-parb) sm) before
  Tuple after para <- parse parseParenthesisLevel $ split.after 
  right <- (parse $ phrase para sm) after 
  pure $ Tuple (fst right) {
    before: reverse $ fst left,
    left: (reverse $ snd left) <> repeat (-parb) ')', 
    right: repeat para '(' <> snd right
  }

exponent :: ∀ e. ParserError e => SymbolMap -> Parser e PartPart
exponent = part '^'

exponent' :: SymbolMap -> Parser String PartPart 
exponent' = exponent

division :: ∀ e. ParserError e => SymbolMap -> Parser e PartPart
division = part '/' 

division' :: SymbolMap -> Parser String PartPart
division' = part '/' 

flatText :: String -> Maybe TextEquation -> TextEquation 
flatText main right = TextEquation {main, right, above: Nothing, below: Nothing}

first :: ∀ e a. ParserError e => (String -> Boolean) -> Parser e a -> Parser e a -> Parser e a 
first b p1 p2 = Parser \s -> if b s then (parse p2 s) <|> (parse p1 s) else (parse p1 s) <|> (parse p2 s)

textEquation :: SymbolMap -> Parser String TextEquation
textEquation sm = expdiv <|> eat
  where 
    expdiv = first (\s -> (indexOf (Pattern "^") s) > (indexOf (Pattern "/") s)) exp div
    exp = Parser \s -> do
      (Tuple after result) <- (parse $ exponent' sm) $ s
      above <- Just <<< snd <$> defer text result.right
      right <- Just <<< snd <$> defer text after
      pure $ Tuple "" (flatText result.before $ texteq {main: result.left, below: Nothing, above, right})
    div = Parser \s -> do
      (Tuple after result) <- (parse $ division' sm) $ s
      below <- Just <<< snd <$> defer text result.right
      right <- Just <<< snd <$> defer text after
      pure $ Tuple "" (flatText result.before $ texteq {main: result.left, below, above: Nothing, right})
    eat = Parser \s -> do
      pure $ Tuple "" (flatText s Nothing)
    text = \_ -> parse $ textEquation sm
    texteq = Just <<< TextEquation