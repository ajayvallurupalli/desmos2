module Equation.Operators
  ( addop
  , divop
  , noSymbols
  , powop
  , sinop
  , symbols
  )
  where

import Prelude

import Data.Array (foldr)
import Data.Either (Either(..))
import Data.Map (empty, insert)
import Data.Number (cos, e, pi, pow, sin, tan)
import Data.Tuple (uncurry, Tuple(..))
import Equation.Expression (Expression(..), Operator(..), SymbolMap(..), VariableType(..), mulop, opName, transformBinary, transformBinary', transformUnary, unionSymbols, value, variable)

safeDiv :: Number -> Number -> Either String Number
safeDiv _ 0.0 = Left "Error: Divide by Zero"
safeDiv x y = pure (x / y)

divop :: Operator
divop = Binary $ {op: transformBinary' safeDiv "divide", precedence: 10, parenthesisLevel: 0, infix: true, name: "/"}

minop :: Operator
minop = Binary $ {op: transformBinary (\x y -> add x (negate y)) "subtract", precedence: 10, parenthesisLevel: 0, infix: true, name: "-"}

addop :: Operator 
addop = Binary $ {op: transformBinary add "add", precedence: 6, parenthesisLevel: 0, infix: true, name: "+"}

powop :: Operator
powop =  Binary $ {op: transformBinary pow "exponentiate", precedence: 14, parenthesisLevel: 0, infix: true, name: "^"}

baseUnary :: String -> (Number -> Number) -> Operator
baseUnary name op = Unary $ {op: transformUnary op name, precedence: 20, parenthesisLevel: 0, name}

sinop :: Operator 
sinop = baseUnary "sin" sin

cosop :: Operator 
cosop = baseUnary "cos" cos

tanop :: Operator 
tanop = baseUnary "tan" tan

invert :: ∀ a. EuclideanRing a => a -> a 
invert x = one / x

cscop :: Operator 
cscop = baseUnary "csc" (invert <<< sin)

secop :: Operator 
secop = baseUnary "sec" (invert <<< cos)

cotop :: Operator 
cotop = baseUnary "cot" (invert <<< tan)

pival :: Expression 
pival = value pi

eval :: Expression 
eval = value e

noSymbols :: SymbolMap 
noSymbols = SymbolMap $ empty

createSymbolMap :: Array Operator -> SymbolMap 
createSymbolMap os = SymbolMap $ 
  foldr (\o acc -> insert (opName o) (Operator o) acc) empty os

trigSymbols :: SymbolMap 
trigSymbols = createSymbolMap [sinop, cosop, tanop, cscop, secop, cotop]

basicOperators :: SymbolMap 
basicOperators = createSymbolMap [mulop, addop, minop, divop, powop]

realNumbers :: SymbolMap 
realNumbers = SymbolMap $ foldr (uncurry insert) empty [Tuple "pi" pival, Tuple "e" eval, Tuple "π" pival]

variables :: SymbolMap 
variables = SymbolMap $ foldr (uncurry insert) empty [Tuple "x" (variable "x" Value'), Tuple "y" (variable "y" Value')]

symbols :: SymbolMap
symbols = foldr unionSymbols noSymbols [trigSymbols, basicOperators, realNumbers, variables]