module Parse
  ( ParsePart(..)
  , cutChars
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
  (Tuple l _) <- (P.parse $ P.many' $ P.satisfy' (codePointFromChar >>> isSpace)) s
  Right $ Tuple l unit

parseLetters :: ∀ e. P.ParserError e => P.Parser e ParsePart
parseLetters = (P.some' $ P.satisfy' (isLetterOrSymbol && not isParenthesis))
  >>= \cs -> pure $ Letter (fromCharArray cs)

cutChars :: String -> Either String (Array ParsePart)
cutChars = 
  let 
    parser :: P.Parser String (Array ParsePart)
    parser = P.parseUntil' $ parseSpace *> (parseDigit <|> parseLetters <|> parseParenthesis)
  in 
    P.parse parser >>> P.unwrap

