module Equation
  ( Calculation(..)
  , Equation(..)
  , emptyVariable
  , expressCalc
  , makeCalculation
  , makeEquation
  , module Equation.Operators
  , runCalculation
  )
  where

import Prelude

import Data.Array (cons, foldr, head, length, tail)
import Data.Array (splitAt) as A
import Data.Either (Either(..), note)
import Data.Foldable (foldl)
import Data.Function.Uncurried (Fn1, mkFn1, runFn1)
import Data.Map (empty, insert)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.CodeUnits (indexOf, splitAt)
import Data.Tuple (Tuple(..), uncurry)
import Equation.Expression (Error, Expression(..), Operator(..), SymbolMap, VariableMap(..), parse, runExpressions, value)
import Equation.Operators (symbols, noSymbols)
import Equation.Parse (ParsePart(..), cutChars)

data Equation 
  = Unbinded Calculation
  | Binded String (Array String) Calculation 
  | Math Number 

instance showEquation :: Show Equation where
  show (Unbinded _) = "Equation: Unbinded "
  show (Binded str _ _) = "Equation: " <> str 
  show (Math n) = "Equation: " <> show n

newtype Calculation = Calculation (Fn1 VariableMap (Either Error Number))

emptyVariable :: VariableMap 
emptyVariable = VariableMap $ empty

-- I think this should mean that it does not parse every time?
makeCalculation :: SymbolMap -> String -> Calculation
makeCalculation symbols string = 
  case parse symbols string of
    Left err -> Calculation $ mkFn1 (\_ -> Left err)
    Right es -> Calculation $ mkFn1 (\variables -> runExpressions es variables)

runCalculation :: Calculation -> VariableMap ->  Either Error Number
runCalculation (Calculation e) variables  = runFn1 e variables

processArguments :: Array ParsePart -> Either String (Array String)
processArguments = foldl aux (pure [])
  where
    aux acc e = 
      case e of
        Letter str -> cons str <$> acc 
        Comma  -> acc 
        _ -> Left "Error: invalid argument."

makeEquation :: SymbolMap -> String -> Either String Equation 
makeEquation symbols str = 
  let 
    parenthesisError =  "Error: Function definition does not close parenthesis."
    invalidSyntax = "Error: Equation must be of the form name(arg1, arg2) = expression."
    invalidName = "Error: invalid function name."
  in
  case indexOf (Pattern "=") str of
    Just i -> do
      let split = splitAt i str
      parts <- cutChars split.before 
      expressions <- parse symbols split.after 
      let tail1 = tail parts
      let tail2 = tail1 >>= tail
      case head parts, tail1 >>= head, tail2 >>= head of
        Nothing, Nothing, Nothing -> Left invalidSyntax
        Just (Letter "y"), _, _ -> pure $ Unbinded (Calculation $ mkFn1 (\vars -> runExpressions expressions vars))
        Just (Letter name), Just (Parenthesis 1), Just (Parenthesis (-1)) -> 
          pure $ Binded name [] (Calculation $ mkFn1 (\vars -> runExpressions expressions vars))
        Just (Letter name), Just (Parenthesis 1), Just (Letter _) -> do
          tail3 <- note parenthesisError (tail2 >>= tail) 
          let cutend = A.splitAt (length tail3 - 1) tail3 
          case head cutend.after of 
            Just (Parenthesis (-1)) -> do
              arguments <- (processArguments cutend.before)
              pure $ Binded name arguments (Calculation $ mkFn1 (\vars -> runExpressions expressions vars))
            _ ->  Left parenthesisError
        _, _, _ -> Left invalidName
    Nothing -> do
      expressions <- parse symbols str 
      let calculation = Calculation $ mkFn1 (\vars -> runExpressions expressions vars)
      pure $ Unbinded calculation

--turns a expression into a calculation
expressCalc :: Calculation -> String -> Array String -> Expression 
expressCalc (Calculation calc) name vars = Operator $ Any { n: (length vars), op, name, precedence: 50, parenthesisLevel: 0 }
  where 
    op es = 
      if length vars /= length es then Left "Error: invalid number of arguments."
      else 
        let variables = VariableMap $ foldr (uncurry insert) empty (Tuple <$> vars <*> es) in 
        value <$> runFn1 calc variables
