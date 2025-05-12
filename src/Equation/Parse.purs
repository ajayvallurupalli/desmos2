module Equation.Parse
  ( ParsePart(..)
  , cutChars
  , parseName
  )
  where

import Prelude

import Control.Alt ((<|>))
import Data.Array (foldl)
import Data.CodePoint.Unicode (isDecDigit, isLetter, isPunctuation, isSpace, isSymbol)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Data.Show.Generic (genericShow)
import Data.String.CodePoints (CodePoint, codePointFromChar)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Parser as P

data ParsePart 
  = Digit Number
  | Parenthesis Int
  | Letter String
  | Comma

derive instance genericParsePart :: Generic ParsePart _
derive instance eqParsePart :: Eq ParsePart

instance showParsePart :: Show ParsePart where
  show = genericShow

isParenthesis :: Char -> Boolean 
isParenthesis c = 
  char == lp || char == rp
  where
    char = codePointFromChar c
    lp = codePointFromChar ')'
    rp = codePointFromChar '('

isLetterOrSymbol :: Char -> Boolean
isLetterOrSymbol = codePointFromChar >>> (isLetter || isPunctuation || isSymbol)

isPeriod :: CodePoint -> Boolean
isPeriod c = c == codePointFromChar '.' 

isDigit :: Char -> Boolean 
isDigit = codePointFromChar >>> (isDecDigit || isPeriod)

isComma :: Char -> Boolean 
isComma = codePointFromChar >>> (_ == codePointFromChar ',')

isSpace' :: Char -> Boolean 
isSpace' = (codePointFromChar >>> isSpace)

parseNumber :: ∀ e. P.Parser e Number
parseNumber = P.Parser \s -> do
  p <- (P.parse $ P.some' $ P.satisfy' isDigit) s
  let (Tuple l cs) = p
  let str = fromCharArray cs
  case fromString str of 
    Nothing -> Left $ P.parseFail $ "Failed to parse " <> str
    Just n -> Right $ Tuple l n
  
parseDigit :: ∀ e. P.Parser e ParsePart
parseDigit = P.Parser \s -> do
  (Tuple l n) <- P.parse parseNumber s
  Right $ Tuple l (Digit n)

parseParenthesis :: ∀ e. P.Parser e ParsePart
parseParenthesis = P.Parser \s -> do
  (Tuple l ps) <- (P.parse $ P.some' $ P.satisfy' isParenthesis) s
  Right $ Tuple l  (Parenthesis (aux ps))
  where 
  aux = foldl (\acc e -> if e == '(' then acc + 1 else acc - 1) 0

parseSpace :: ∀ e. P.Parser e Unit
parseSpace = P.Parser \s -> do
  (Tuple l _) <- (P.parse $ P.many' $ P.satisfy' isSpace') s
  Right $ Tuple l unit

parseComma :: ∀ e. P.ParserError e => P.Parser e ParsePart 
parseComma = P.satisfy' isComma >>= \_ -> pure Comma

parseLetters :: ∀ e. P.ParserError e => P.Parser e ParsePart
parseLetters = (P.some' $ P.satisfy' (isLetterOrSymbol && not isParenthesis && not isComma))
  >>= \cs -> pure $ Letter (fromCharArray cs)

cutChars :: String -> Either String (Array ParsePart)
cutChars = 
  let 
    parser :: P.Parser String (Array ParsePart)
    parser = P.parseUntil' $ parseSpace *> (parseComma <|> parseDigit <|> parseLetters <|> parseParenthesis)
  in 
    P.parse parser >>> P.unwrap

parseName :: ∀ e. Eq e => P.Parser e String 
parseName = P.Parser \s -> do
  (Tuple s2 res) <- P.parse (P.some' $ P.satisfy' (isLetterOrSymbol || isDigit)) s
  (Tuple done _) <- P.parse (P.parseUntil' $ (P.some' $ P.satisfy' isSpace')) s2 --it throws away this just to make sure 
  pure $ Tuple done (fromCharArray res)