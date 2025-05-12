module Components.Canvas where

import Prelude

import Capability.Equation (class ManageEquation)
import Capability.Log (class LogMessage, Log(..), log)
import Components.Box as Box
import Components.Column as Column
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (length)
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type State = {}

data Action 
  = Initialize
  | HandleKey KE.KeyboardEvent
  | SelectCanvas

type ChildSlot = (column :: Column.Slot Box.Query Box.Output Box.Input Unit)

column = Proxy :: Proxy "column"

component :: ∀ q i o m. ManageEquation m => MonadAff m => LogMessage m => H.Component q i o m
component = 
  H.mkComponent 
    { initialState 
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction 
      , initialize = Just Initialize
      }
    }
render :: ∀ m. ManageEquation m => LogMessage m => State -> H.ComponentHTML Action ChildSlot m 
render _ = 
  HH.div [HP.classes [ClassName "row", ClassName "full"]]
  [ HH.slot_ Column._column unit boxColumn unit
  , HH.canvas [HP.class_ $ ClassName "canvas", HE.onClick $ const SelectCanvas]
  ]

  where
    boxColumn :: ∀ m1 b. ManageEquation m1 => LogMessage m1 => H.Component (Column.Query Box.Query Box.Input) b (Column.Output Box.Output) m1
    boxColumn = Column.column Box.Unfocus Box.GetContents Box.Focus [] Box.component

initialState :: ∀ i. i -> State
initialState = const {}

handleAction :: ∀ o m. MonadAff m => LogMessage m => Action -> H.HalogenM State Action ChildSlot o m Unit 
handleAction = case _ of
  Initialize -> do
    document <- H.liftEffect $ document =<< window 
    _ <- H.subscribe $
      eventListener
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map HandleKey <<< KE.fromEvent)

    pure unit

  HandleKey ev -> do 
    log $ Debug (KE.key ev)
    if KE.key ev == "Enter" then H.tell Column._column unit (Column.QInsertBelow Box.emptyInput)
    else if KE.key ev == "ArrowLeft" then H.tell Column._column unit (Column.QuerySelected <<< Box.Leftarrow)
    else if KE.key ev == "ArrowRight" then H.tell Column._column unit (Column.QuerySelected <<< Box.Rightarrow)
    else if length (KE.key ev) == 1 then H.tell Column._column unit (Column.QuerySelected <<< (Box.Typed (KE.key ev)))
    else log $ Debug $ KE.key ev 

  SelectCanvas -> H.tell Column._column unit (Column.Unfocus)