module Data.Difference where

import qualified "base" Prelude as Base

data Difference a = LT' a | EQ' | GT' a
  deriving (Functor, Base.Eq, Show)
