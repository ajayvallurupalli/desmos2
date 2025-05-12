module Capability.Equation
  ( Id(..)
  , SymbolError(..)
  , class ManageEquation
  , deleteEquation
  , getSymbolMap
  , registerEquation
  , updateEquation
  )
  where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Equation.Expression (SymbolMap)
import Halogen (HalogenM, lift)

newtype Id = Id Int
derive instance eqId :: Eq Id 
derive instance ordId :: Ord Id
derive instance newtypeId :: Newtype Id _
derive newtype instance showId :: Show Id

data SymbolError 
  = DoesNotExist
  | ParseError String
  | AlreadyExists String

class Monad m <= ManageEquation m where
  getSymbolMap :: Unit -> m SymbolMap 
  registerEquation :: Unit -> m Id
  updateEquation :: Id -> String -> m (Either SymbolError (Maybe Number))
  deleteEquation :: Id -> m Unit

instance manageEquationHalogenM :: ManageEquation m => ManageEquation (HalogenM st act slots msg m) where
  getSymbolMap = lift <<< getSymbolMap
  registerEquation = lift <<< registerEquation
  updateEquation id = lift <<< updateEquation id
  deleteEquation = lift <<< deleteEquation