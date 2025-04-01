module Main where

{-
"(1+(2+3)+4+(5+6))+f(1, 2)"  
(Value 1) "+(2+3)+4+(5+6))+f(1)""                   [Eats 1, continues]
(Binary (Value 1) + ) "(2+3)+4+(5+6))+f(1)""        [Eats +, wraps previous in Binary because + is Binary]

REC
(2+3)                                               [Sees parenthesi, starts a new parse]
-}

import Prelude

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log $ show $ 1 + 2
