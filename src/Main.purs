module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Equation (emptyVariable, makeEquation, runEquation, symbols)

main :: Effect Unit
main = do
  let equation = makeEquation symbols "(((3-(2+(1/(4)))))^(3))+(2/(5))"
  log $ show $ runEquation equation emptyVariable
