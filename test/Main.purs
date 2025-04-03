module Test.Main where

import Prelude

import Control.Plus (empty)
import Data.Array (foldr)
import Data.Either (Either(..))
import Data.Map (insert)
import Data.Number (pi, sin)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Expression (Expression(..), Operator(..), addop, deleteExtraMultiplies, mulop, parse, runExpressions, sinop, transformBinary, transformUnary, variable)
import Parse (ParsePart(..), cutChars)
import Test.Spec (describe, it, pending')
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
  describe "Tokenize" do
    it "Empty" do
      cutChars "" `shouldEqual` Right []
    it "Int" do
      cutChars "123" `shouldEqual` Right [Digit 123.0]
    it "Number" do
      cutChars "123.0" `shouldEqual` Right [Digit 123.0]
    it "Letters" do 
      cutChars "abc" `shouldEqual` Right [Letter "abc"]
    it "Special character" do 
      cutChars "πΨψ" `shouldEqual` Right [Letter "πΨψ"]
    it "Parenthesis" do
      cutChars "(()" `shouldEqual` Right [Parenthesis 1]
    it "Everything" do
      cutChars "123(a+b)" `shouldEqual` Right [Digit 123.0, Parenthesis 1, Letter "a+b", Parenthesis (-1)]
    it "Invalid character" do
      cutChars "‎" `shouldEqual` Left "''‎'' is an invalid character."
  describe "Parse" do 
    let symbols = foldr (uncurry insert) empty [Tuple "*" (Operator mulop), Tuple "sin" (Operator sinop), Tuple "+" (Operator addop), Tuple "pi" (Value 3.14)]
    it "Empty" do
      parse symbols "" `shouldEqual` Right []
    it "Number" do
      parse symbols "123" `shouldEqual` Right [Value 123.0]
    it "Constant" do
      parse symbols "pi" `shouldEqual` Right [Value 3.14]
    it "Binary" do
      parse symbols "*" `shouldEqual` Right [Operator mulop]
    it "Parenthesis depth 1" do
      parse symbols "(*)" `shouldEqual` Right [Operator $ Binary $ {op: transformBinary mul "multiply", precedence: 10, parenthesisLevel: 1, infix: true, name: "*"}]
    it "Unary" do
      parse symbols "sin" `shouldEqual` Right [Operator sinop]
    it "Binary Unary" do
      parse symbols "2*sin(3.0)" `shouldEqual` Right [Value 2.0, Operator mulop, Operator sinop, Value 3.0]
    it "Basic equation" do
      parse symbols "123*456" `shouldEqual` Right [Value 123.0, Operator mulop, Value 456.0]
    it "Implicit multiplication from parenthesis" do
      parse symbols "123(456)" `shouldEqual` Right [Value 123.0, Operator mulop, Value 456.0]
    it "Only implicit multiply when there's no operator" do
      parse symbols "123*(456)" `shouldEqual` Right [Value 123.0, Operator mulop, Value 456.0]
    pending' "Implict multiplication from variable" do
      parse symbols "3pi" `shouldEqual` Right [Value 3.0, Operator mulop, Value 3.14]
    describe "Other" do
      it "deleteExtraMultiplies 1" do
        ((parse symbols "sin*32*sin*") <#> deleteExtraMultiplies) `shouldEqual` Right [Operator sinop, Value 32.0, Operator mulop, Operator sinop]
      it "deleteExtraMultiplies 2" do
        ((parse symbols "123*456") <#> deleteExtraMultiplies) `shouldEqual` Right [Value 123.0, Operator mulop, Value 456.0]
  describe "MATH!" do
    let symbols = foldr (uncurry insert) empty [
        Tuple "*" (Operator mulop)
      , Tuple "sin" (Operator sinop)
      , Tuple "+" (Operator addop)
      , Tuple "pi" (Value pi)
      , Tuple "x" (variable "x")
      , Tuple "si" (variable "si")
      , Tuple "o" (variable "o")
    ]
    let variables = foldr (uncurry insert) empty [Tuple "x" (Value 3.0), Tuple "si" (Value 100.0), Tuple "o" (Operator sinop)]
    it "Binary" do
      (parse symbols "123*456" >>= 
        \es -> runExpressions es empty) `shouldEqual` pure (123.0 * 456.0)
    it "Binary Pemdas 1" do
      (parse symbols "3+7*2" >>= 
        \es -> runExpressions es empty) `shouldEqual` pure (3.0 + 7.0 * 2.0)
    it "Binary Pemdas 2" do
      (parse symbols "7*2+3" >>= 
        \es -> runExpressions es empty) `shouldEqual` pure (7.0 * 2.0 + 3.0)
    it "Distribute 1" do
      (parse symbols "3*(1+2)" >>= 
        \es -> runExpressions es empty) `shouldEqual` pure (3.0 * (1.0 + 2.0))
    it "Distribute 2" do
      (parse symbols "3(1+2)" >>= 
        \es -> runExpressions es empty) `shouldEqual` pure (3.0 * (1.0 + 2.0))
    it "Unary" do
      (parse symbols "sin(3.0)" >>= 
        \es -> runExpressions es empty) `shouldEqual` pure (sin 3.0)
    it "Binary and Unary 1a" do
      (parse symbols "2*(sin(3.0))" >>= 
        \es -> runExpressions es empty) `shouldEqual` pure (2.0 * (sin 3.0))
    it "Binary and Unary 1b" do
      (parse symbols "2*(sin(3.0))" >>= 
        \es -> runExpressions es empty) `shouldEqual` pure (2.0 * (sin 3.0))
    it "Binary and Unary 2" do
      (parse symbols "sin(3.0)*2" >>= 
        \es -> runExpressions es empty) `shouldEqual` pure (2.0 * (sin 3.0))
    it "Binary and Unary 2" do
      (parse symbols "sin(3.0*2.0)" >>= 
        \es -> runExpressions es empty) `shouldEqual` pure (sin 6.0)
    it "Constants" do
      (parse symbols "sin(pi*0.5)" >>= 
        \es -> runExpressions es empty) `shouldEqual` pure 1.0 --sin(pi/2)
    it "Variables" do
      (parse symbols "3*x+2" >>= 
        \es -> runExpressions es variables) `shouldEqual` pure (3.0 * 3.0 + 2.0)--sin(pi/2)
    it "Function and Variable 1" do
      (parse symbols "sin(x)" >>= 
        \es -> runExpressions es variables) `shouldEqual` pure (sin 3.0)
    it "Function and Variable 2" do
      (parse symbols "sinx" >>= 
        \es -> runExpressions es variables) `shouldEqual` pure (sin 3.0)
    it "Variable Heaven" do
      (parse symbols "si(osi)" >>= 
        \es -> runExpressions es variables) `shouldEqual` pure (100.0 * (sin 100.0))
    describe "Loco" do
      let varop _ = Right $ (variable "x")
      let var = Unary $ {op: varop, precedence: 20, parenthesisLevel: 0, name: "var"}
      let newSymbols = insert "var" (Operator var) symbols
      it "Loco" do
        (parse newSymbols "var 1" >>= 
          \es -> runExpressions es variables) `shouldEqual` pure 3.0
      it "Loco Loco" do
        (parse newSymbols "o(var 1)" >>= 
          \es -> runExpressions es variables) `shouldEqual` pure (sin 3.0)
    
      
