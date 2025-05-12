module Components.Column
  ( Action(..)
  , ChildSlot
  , Input
  , Output(..)
  , Query(..)
  , Slot
  , State
  , _column
  , _inner
  , column
  )
  where

import Prelude

import Capability.Log (class LogMessage, Log(..), log)
import Data.Array (cons, insertAt, length, mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), uncurry)
import Data.Typelevel.Undefined (undefined)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Slot f o i a = H.Slot (Query f i) (Output o) a

data Query f i a
  = QuerySelected (f a)
  | QueryInner Int (f a)
  | QInsert Int i a
  | QInsertBelow i a
  | Unfocus a

data Output o
  = Bubble o

data Action o i
  = HandleInner Int o 
  | Insert Int i
  | InsertBelow i 

type State i q o = 
  { inputs :: Array (Tuple Int i)
  , nextId :: Int
  , selected :: Maybe Int
  , unfocus :: H.Tell q
  , download :: H.Request q i 
  , focus :: o
  }

type Input i q o = 
  { unfocus :: H.Tell q
  , download :: H.Request q i
  , focus :: o
  , inputs :: Array i
  }

type ChildSlot f o = ( inner :: H.Slot f o Int)

_inner = Proxy :: Proxy "inner"

_column = Proxy :: Proxy "column"

column :: ∀ q i o m b. Eq o => LogMessage m => Show i =>
  H.Tell q -> H.Request q i -> o -> Array i -> H.Component q i o m -> H.Component (Query q i) b (Output o) m
column unfocus download focus inputs inner = 
  H.mkComponent 
    { initialState: initialState {unfocus, download, focus, inputs}
    , render: render inner 
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
    }

initialState :: ∀ i q o b. Input i q o -> b -> State i q o
initialState {unfocus, download, focus} _ = { inputs: [], nextId: 0, selected: Nothing, unfocus, download, focus }

render :: ∀ q i o m. 
  H.Component q i o m -> State i q o -> H.ComponentHTML (Action o i) (ChildSlot q o) m
render inner state = 
  HH.div [HP.class_ $ ClassName "column"] $
    map (uncurry mkInner) state.inputs 
  where
    mkInner id i = HH.slot _inner id inner i (HandleInner id)

handleAction :: ∀ q i o m. Eq o => LogMessage m => Show i =>
  Action o i -> H.HalogenM (State i q o) (Action o i) (ChildSlot q o) (Output o) m Unit 
handleAction = case _ of
  HandleInner id o ->
    H.gets _.focus >>= \focus ->
      if o == focus then do 
        selected <- H.gets _.selected 
        case selected of
          Nothing -> H.modify_ \s -> s { selected = Just id}
          Just x -> do
            unfocus <- H.gets _.unfocus 
            H.tell _inner x unfocus
            log $ Debug $ show id <> " was selected."
            H.modify_ \s -> s { selected = Just id}  
      else H.raise $ Bubble o
  Insert at i -> do
    log $ Debug $ "Inserting box at " <> show at
    { download, inputs, nextId } <- H.get
    newinputs <- sequence $ map (\(Tuple ind _) -> Tuple ind <$> H.request _inner ind download) inputs -- Array (Tuple Int (Maybe i))
    let newinputs' = sequence $ map sequence newinputs -- Maybe (Array (Tuple Int))
    --log $ Debug $ "New inputs from inserting:  " <> show i <> " is " <> show (insertAt at (Tuple nextId i) newinputs')
    H.modify_ \s -> s { inputs = fromMaybe' (\_ -> undefined) (insertAt at (Tuple nextId i) =<< newinputs'), nextId = nextId + 1}
  InsertBelow i -> do
    selected <- H.gets _.selected
    inputs <- H.gets _.inputs
    log $ Debug $ show selected
    case selected of
      Nothing -> handleAction $ Insert (length inputs) i
      Just x -> handleAction $ Insert (x + 1) i

handleQuery :: ∀ q i o m a. Eq o => LogMessage m => Show i =>
  Query q i a -> H.HalogenM (State i q o) (Action o i) (ChildSlot q o) (Output o) m (Maybe a)
handleQuery = case _ of 
  QuerySelected q -> do
    selected <- H.gets _.selected
    case selected of
      Nothing -> pure Nothing --i think this represents failure..? idk
      Just id -> H.query _inner id q
  QueryInner id q -> H.query _inner id q
  QInsert at i a -> do
    handleAction $ Insert at i
    pure $ Just a
  QInsertBelow i a -> do
    log $ Debug "QInsert Below"
    handleAction $ InsertBelow i
    pure $ Just a
  Unfocus a -> do 
    selected <- H.gets _.selected 
    unfocus <- H.gets _.unfocus
    case selected of
      Nothing -> pure unit 
      Just i -> H.tell _inner i unfocus
    H.modify_ \s -> s { selected = Nothing }
    pure $ Just a 