module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Parse (ParsePart(..), cutChars)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
  describe "parse" do
    it "parse empty" do
      cutChars "" `shouldEqual` Right []
    it "parse number" do
      cutChars "123" `shouldEqual` Right [Digit 123.0]
    it "parse letters" do 
      cutChars "abc" `shouldEqual` Right [Letter "abc"]
    it "special character" do 
      cutChars "πΨψ" `shouldEqual` Right [Letter "πΨψ"]
    it "parse parenthesis" do
      cutChars "(()" `shouldEqual` Right [Parenthesis 1]
    it "parse everything" do
      cutChars "123(a+b)" `shouldEqual` Right [Digit 123.0, Parenthesis 1, Letter "a+b", Parenthesis (-1)]
    it "invalid character" do
      cutChars "\n" `shouldEqual` Left "''\\n'' is an invalid character."
