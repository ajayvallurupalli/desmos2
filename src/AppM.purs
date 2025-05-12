module AppM where

import Prelude

import Capability.Equation (class ManageEquation, Id(..), SymbolError(..))
import Capability.Log (class LogMessage, Log(..))
import Data.Either (Either(..))
import Data.Map (Map, delete, empty, insert, lookup, pop, update)
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Undefined (undefined)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log) as Console
import Equation (Equation(..), expressCalc, makeEquation)
import Equation.Expression (SymbolMap(..))
import Equation.Operators (symbols)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT, updateStore)
import Safe.Coerce (coerce)

data Level = Development | Production
derive instance eqLogLevel :: Eq Level
instance showLevel :: Show Level where
  show Development = "Development"
  show Production = "Production"

type EquationData = 
  { equation :: Equation 
  , string :: String
  , enabled :: Boolean
  }

type Store = 
  { logLevel :: Level
  , symbols :: SymbolMap
  , register :: Map Id (Maybe EquationData)
  , names :: Map String Id
  , nextId :: Id
  }

emptyStore :: Store
emptyStore =  
  { logLevel:  Development 
  , symbols: symbols
  , register: empty
  , names: empty
  , nextId: Id 0
  }

type Action = Store -> Store

newtype AppM a = AppM (StoreT Action Store Aff a)

runAppM :: ∀ q i o. Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM init = runStoreT init reduce <<< coerce

reduce :: Store -> Action -> Store 
reduce store k = k store

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadStoreAppM :: MonadStore Action Store AppM

instance logMessageAppM :: LogMessage AppM where
  log l = do
    { logLevel } <- getStore
    case logLevel, l of
      Development, Debug str -> Console.log $ formatDebug str
      Development, Error dt str -> Console.log $ formatError dt str
      Production, Error _ _ -> undefined -- this will error and make me remember
      _, _ -> pure unit
    where
      formatDebug str = "[Debug: " <> str <> "]"
      formatError time str = "[Error@" <> show time <> " : " <> str <> "]"
  dumpState _ = do
    store <- getStore 
    Console.log $ show store
    pure unit

updateEquation :: (EquationData -> EquationData) -> Id -> Map Id (Maybe EquationData) -> Map Id (Maybe EquationData)
updateEquation up = update (\ed -> Just $ up <$> ed) 

updateEquation' :: (EquationData -> EquationData) -> Id -> AppM Unit
updateEquation' up id = updateStore \s -> s { register = updateEquation up id s.register}

clearEquation :: Id -> Map Id (Maybe EquationData) -> Map Id (Maybe EquationData)
clearEquation = update $ const Nothing

clearEquation' :: Id -> AppM Unit
clearEquation' id = updateStore \s -> s { register = clearEquation id s.register}

getName :: Id -> AppM (Maybe String)
getName id = do
  { register } <- getStore
  case lookup id register of 
    Just (Just {equation: (Binded name _ _)}) -> pure $ pure name
    _ -> pure Nothing

rename :: Id -> String -> AppM Unit 
rename id new = do
  { names } <- getStore  
  n <- getName id
  case n of
    Nothing -> pure unit 
    Just name -> 
      case pop name names of
        Nothing -> updateStore \s -> s { names = insert new id s.names }
        Just (Tuple _ popped) -> updateStore \s -> s { names = insert new id popped }

instance manageSymbolAppM :: ManageEquation AppM where
  getSymbolMap _ = getStore <#> (\s -> s.symbols)
  registerEquation _ = do
    { register, nextId } <- getStore 
    updateStore \s -> s { nextId = over Id (_ + 1) nextId
                        , register = insert nextId Nothing register }
    pure nextId
  deleteEquation id = updateStore \s -> s { register = delete id s.register}
  updateEquation id str = do
    { symbols, names } <- getStore  
    case makeEquation symbols str of
      Left err -> do
        clearEquation' id 
        pure $ Left $ ParseError err
      Right x -> do 
        updateEquation' (\ed -> ed { equation = x, string = str }) id
        case x of
          Math n -> pure $ pure $ pure n --amazing coding skills
          Unbinded _ -> pure $ pure Nothing
          Binded name args calculation ->
            case pop name names of
              Nothing -> pure $ pure $ Nothing                  
              Just (Tuple own _) ->
                if id /= own then pure $ Left $ AlreadyExists name
                else do
                  rename id name
                  updateStore \s -> s { symbols = over SymbolMap (insert name (expressCalc calculation name args)) s.symbols }
                  pure $ pure $ Nothing                  
            