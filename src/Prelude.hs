module Prelude (module A) where

import "base" Prelude as A (Functor (..), Ordering (..), Read, Show, (<$>), ($), curry, otherwise)
import Algebra as A
import Control.Applicative as A (Applicative (..), Alternative (..), optional)
import Control.Category as A (Category (..))
import Data.Bool as A (Bool (..), bool, not)
import Data.Foldable as A (Foldable (..), all, any)
import Data.Maybe as A (Maybe (..), fromMaybe, maybe)
import Data.Monoid as A (Product (..), Sum (..))
import Numeric.Natural as A (Natural)
import Relation.Binary.Comparison as A
