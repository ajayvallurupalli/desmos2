module Result where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)

newtype Result a b = Result (Either a b)

derive newtype instance functorResult :: Functor (Result a)
derive instance genericResult :: Generic (Result a b) _

instance applyResult :: (Semigroup s) => Apply (Result s) where
  apply (Result (Left a))    (Result (Left b))   = Result $ Left $ a <> b
  apply (Result (Left a))    _                   = Result $ Left a
  apply _                    (Result (Left b))   = Result $ Left b
  apply (Result (Right f))   (Result x)          = Result $ f <$> x

instance applicativeResult :: (Semigroup s) => Applicative (Result s) where
  pure = Result <<< Right

--I'm pretty sure this is all good with alt laws, since they are not commutative anywat
instance altResult :: (Semigroup s) => Alt (Result s) where
  alt (Result (Left l)) (Result (Left r))  = Result $ Left $ l <> r
  alt (Result (Left _)) r                  = r
  alt l                 _                  = l -- TO favor left side

-- I never really understood why either didn't have a plus instance with mempty
-- since it follows the identity laws due to either favoring right,
-- and it follows Annihilation by since monoid empty follows annihilation
instance plusResult :: (Monoid s) => Plus (Result s) where
  empty = Result $ Left mempty