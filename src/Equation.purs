module Equation
  ( Equation(..)
  , emptyVariable
  , makeEquation
  , runEquation
  , module Operators
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, mkFn1, runFn1)
import Data.Map (empty)
import Expression (Error, SymbolMap, VariableMap(..), parse, runExpressions)
import Operators (symbols, noSymbols)

newtype Equation = Equation (Fn1 VariableMap (Either Error Number))

emptyVariable :: VariableMap 
emptyVariable = VariableMap $ empty

-- I think this should mean that it does not parse every time?
makeEquation :: SymbolMap -> String -> Equation
makeEquation symbols string = 
  case parse symbols string of
    Left err -> Equation $ mkFn1 (\_ -> Left err)
    Right es -> Equation $ mkFn1 (\variables -> runExpressions es variables)

runEquation  :: Equation -> VariableMap ->  Either Error Number
runEquation (Equation e) variables  = runFn1 e variables