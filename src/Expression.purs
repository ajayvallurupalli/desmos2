module Expression where

import Prelude

import Data.Either (Either(..))
import Data.Map (Map)
import Parse (cutChars)

type OperatorData =
  (precedence :: Int, infix :: Boolean)

type BinaryOperator = {
  op :: (Expression -> Expression -> Expression)
  | OperatorData
}

type UnaryOperator = {
  op :: (Expression -> Expression)
  | OperatorData
}

data Expression 
  = Value Number
  | Binary BinaryOperator
  | Unary UnaryOperator

newtype SymbolMap = SymbolMap (Map String Expression)

type Context = {
  symbols :: SymbolMap
}

type State = {
  parenthesis :: Int,
  result :: Array Expression,
  build :: Array Char
}

parse :: String -> Either String Expression
parse s = do
    l <- cutChars s
    pure $ Value 1.0