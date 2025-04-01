module Parse
  ( ParsePart(..)
  , cutChars
  )
  where

import Prelude

import Control.Alt ((<|>))
import Data.Array (uncons)
import Data.CodePoint.Unicode (isDecDigit, isLetter, isPunctuation, isSymbol)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
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

isDigit :: Char -> Boolean 
isDigit = codePointFromChar >>> isDecDigit

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
  
  let 
    aux :: Int -> Array Char -> Int
    aux acc lst =
      case uncons lst of 
        Nothing -> acc
        Just {head, tail} -> 
          if head == '(' then aux (acc + 1) tail 
          else aux (acc - 1) tail

  Right $ Tuple l (Parenthesis $ aux 0 ps)

parseLetters :: ∀ e. P.ParserError e => P.Parser e ParsePart
parseLetters = (P.some' $ P.satisfy' (isLetterOrSymbol && not isParenthesis))
  >>= \cs -> pure $ Letter (fromCharArray cs)

cutChars :: String -> Either String (Array ParsePart)
cutChars = 
  let 
    parser :: P.Parser String (Array ParsePart)
    parser = P.parseUntil' $ parseDigit <|> parseLetters <|> parseParenthesis
  in 
    P.parse parser >>> P.unwrap

