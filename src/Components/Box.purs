module Components.Box
  ( Action(..)
  , Input
  , Output(..)
  , Query(..)
  , Slot
  , State
  , _box
  , component
  , emptyInput
  , emptyState
  , initialState
  , render
  )
  where

import Prelude

import Capability.Equation (class ManageEquation, Id, deleteEquation, getSymbolMap, registerEquation)
import Capability.Log (class LogMessage, Log(..), log)
import Data.Array (cons, snoc)
import Data.Array (insertAt) as A
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.String (length)
import Data.String.CodeUnits (fromCharArray, splitAt, toCharArray)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Undefined (undefined)
import Equation.Textual (TextEquation(..), debugShow, emptyTextEquation, textEquation)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties as Hp
import Parser (parse)
import Type.Proxy (Proxy(..))

data Action = Selected | Initialize | Finalize | Equate

data Output = Focus

derive instance eqBoxOutput :: Eq Output

data Query a 
  = Typed String a 
  | Unfocus a
  | GetContents (Input -> a)
  | Leftarrow a 
  | Rightarrow a 

type State = {
  contents :: String,
  textual :: Either String TextEquation,
  focus :: Boolean,
  equationId :: Maybe Id,
  cursor :: Int
}

emptyState :: State 
emptyState = {contents: "", textual: Right emptyTextEquation, focus: false, equationId: Nothing, cursor: 0}

type Input = {
  contents :: String,
  equationId :: Maybe Id
}

emptyInput :: Input 
emptyInput = {contents: "x^2", equationId: Nothing}

_box = Proxy :: Proxy "box"

type Slot id = ( box :: H.Slot Query Output id )

initialState :: Input -> State 
initialState {contents, equationId} = emptyState {contents = contents, equationId = equationId}

component :: ∀ m. ManageEquation m => LogMessage m => H.Component Query Input Output m
component = 
  H.mkComponent
    { initialState
    , render 
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Initialize
      , receive = const $ Just Equate
      }
    }

insertAt :: Int -> Char -> String -> String 
insertAt i c s = fromMaybe' (\_ -> fromCharArray $ snoc (toCharArray s) c) (fromCharArray <$> (A.insertAt i c (toCharArray s)))

--first string is inserted to second string at int index
insertAt' :: Int -> String -> String -> String 
insertAt' i s1 s2 = let split = splitAt i s2 in split.before <> s1 <> split.after

--im the greatest coder of all time
equation :: ∀ w i. Maybe TextEquation -> Int -> Int -> Boolean -> Maybe Int -> Tuple (Maybe Int) (HH.HTML w i)
equation Nothing _ _ _ curs = Tuple curs $ HH.span_ [] 
equation (Just (TextEquation text)) up down bar curs =
  case curs of
    Just x | x <= (length text.main) -> 
      Tuple Nothing $ 
                  HH.span [style up] [ HH.text $ insertAt x '>' text.main
                  , snd $ equation text.above (up + 100) down bar Nothing
                  , snd $ equation text.below (up + 6) down bar Nothing
                  , snd $ equation text.right (up + 7) down bar Nothing]
    Just x -> 
      let above = equation text.above (up + 1) down bar (Just $ x - (length text.main)) in 
      let below = equation text.below (up + 2) down bar (fst above) in   
      let right = equation text.right (up + 3) down bar (fst below) in 
      Tuple (fst right) $ HH.span [style up] [ HH.text text.main, snd above, snd below, snd right]
    
    Nothing -> 
      Tuple Nothing $ 
                  HH.span [style up] [ HH.text text.main
                  , snd $ equation text.above (up + 1) down bar Nothing
                  , snd $ equation text.below (up + 4) down bar Nothing
                  , snd $ equation text.right (up + 5) down bar Nothing]
    where
      style up' = HP.style $ "transform: translateY(" <> show up' <> ");"

render :: ∀ m. State -> H.ComponentHTML Action () m
render state =
  HH.div (cons classes [HE.onClick $ const Selected]) 
    [HH.div [Hp.class_ $ ClassName "equation"] [(snd $ equation (hush state.textual) 0 0 true (Just state.cursor))]]
  where
    classes = 
      HP.classes if state.focus then [ClassName "box", ClassName "selected"] else [ClassName "box"]                 

handleAction :: ∀ m. ManageEquation m => LogMessage m => Action -> H.HalogenM State Action () Output m Unit 
handleAction = case _ of
  Equate -> do
    log $ Debug "Equate"
    sm <- getSymbolMap unit
    contents <- H.gets _.contents
    let text = parse (textEquation sm) $ contents
    H.modify_ \s -> s { textual = snd <$> text }
    log $ Debug $ show $ debugShow <$> (snd <$> text)
    pure unit
  Initialize -> do
    log $ Debug "Initialize"
    id <- registerEquation unit
    H.modify_ \s -> s { equationId = Just id }
    handleAction Equate
  Finalize -> do
    log $ Debug "Finalize"
    id <- H.gets _.equationId 
    fromMaybe' (\_ -> pure unit) (deleteEquation <$> id)
  Selected -> do
    state <- H.get
    log $ Debug $ "Selected state: " <> show state
    H.raise Focus
    H.modify_ \s -> s { focus = true }

handleQuery :: ∀ a m. ManageEquation m => LogMessage m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  Typed str a -> do
    log $ Debug ("Typed" <> str)
    H.modify_ \s -> s { contents = insertAt' s.cursor str s.contents --I would try querying rightarrow but idk how to query self
                      , cursor = if s.cursor == (length s.contents + (length str)) then (length s.contents + (length str)) else s.cursor + (length str)} --+ length of added part, since it will change
    handleAction Equate --update equation
    pure $ Just a
    
  Unfocus a -> do
    log $ Debug "Unfocus"
    H.modify_ \s -> s { focus = false }
    pure $ Just a

  GetContents a -> do
    log $ Debug "Get Contents"
    state <- H.get
    let result = { contents: state.contents, equationId: state.equationId}
    pure (Just (a result))
  Leftarrow a -> do
    log $ Debug "Left"
    H.modify_ \s -> s { cursor = if s.cursor == 0 then 0 else s.cursor - 1}
    pure $ Just a
  Rightarrow a -> do
    log $ Debug "Right"
    H.modify_ \s -> s { cursor = if s.cursor == (length s.contents) then (length s.contents) else s.cursor + 1}
    pure $ Just a