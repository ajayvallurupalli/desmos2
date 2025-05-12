module Main where

import Prelude

import AppM (emptyStore, runAppM)
import Components.Canvas as View
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

{-
TODO
Static Analysis of Expression using tags to variables
GRAPHICS!!!
Realize Textual doesn't work and cry 
Use Latex
Take a class on numerical methods
-}

{-
HALOGEN TREE

Canvas - Brains of the operation
Scroll - Dumb widget that only handles creating boxes, and propagates very other message
Box - Only handles displaying equation, sends string up the tree
Settings Button - Button that creates setting. Probably won't be its own component, but just a part of canvas
Settigns Modal - Pops up when settings button is clicked. On close it sends the settings up the brains

-- TODO - figure how to manage global state for settings (especialy with the symbolmap which must be shared by all boxes)
-- since sending the messages up and down won't work
-- I head about using a reader monad, not sure how to


                                        Canvas 
                                      /        \
                                     /          \
                                    /            \
                                  Scroll      Settings Button
                                    |               |
                              Box --|               |
                                    |               |
                              Box --|               |
                                    |               |
                              Box --|         Settings Modal

desired capabilities - 
Log -- Log either to console or to server
AddSymbol - Binds symbol to map  
-}

main :: Effect Unit
main = HA.runHalogenAff do  
  body <- HA.awaitBody
  rootComponent <- runAppM emptyStore View.component
  runUI rootComponent unit body