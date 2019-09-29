module Data.N where

import qualified "base" Prelude as Base

import Data.Difference
import Data.Np hiding (sub')
import qualified Data.Np as Np

data N = Nul
       | Pos Np
  deriving (Base.Eq, Base.Ord)

instance PartialEq N where (≡) = (Base.==)
instance Preord N where (≤) = (Base.<=)
instance Eq N
instance PartialOrd N where tryCompare x y = Just (compare x y)
instance Ord N where compare = Base.compare

instance {-# OVERLAPPING #-} Semigroup (Sum N) where
    Sum Nul <> y = y
    x <> Sum Nul = x
    Sum (Pos x) <> Sum (Pos y) = Pos <$> Sum x <> Sum y

instance {-# OVERLAPPING #-} Monoid (Sum N) where
    mempty = Sum Nul

instance {-# OVERLAPPING #-} Semigroup (Product N) where
    Product Nul <> _ = Product Nul
    _ <> Product Nul = Product Nul
    Product (Pos x) <> Product (Pos y) = (Product . Pos) (x * y)

instance {-# OVERLAPPING #-} Monoid (Product N) where
    mempty = Product (Pos XH)

instance FromInteger N where
    fromInteger 0 = Nul
    fromInteger n = Pos XH + fromInteger (n-1)

un_suivi_de, zero_suivi_de :: N -> N

un_suivi_de' :: N -> Np
un_suivi_de' = \ case
    Nul -> XH
    Pos p -> XI p

un_suivi_de = Pos . un_suivi_de'

zero_suivi_de = \ case
    Nul -> Nul
    Pos p -> Pos (XO p)

double_moins_deux :: Np -> N
double_moins_deux = \ case
    XI x -> Pos (XO (XO x))
    XO x -> Pos (XO (double_moins_un x))
    XH -> Nul

sub_pos :: Np -> Np -> N
sub_pos = curry $ \ case
    (XI x, XI y) -> zero_suivi_de (sub_pos x y)
    (XI x, XO y) -> un_suivi_de (sub_pos x y)
    (XI x, XH)   -> Pos (XO x)
    (XO x, XI y) -> un_suivi_de (sub_pos x y)
    (XO x, XO y) -> zero_suivi_de (sub_pos x y)
    (XO x, XH)   -> Pos (double_moins_un x)
    (XH,   XI y) -> Pos (double_moins_un y)
    (XH,   XO y) -> double_moins_deux y
    (XH,   XH)   -> Nul

sub_neg :: Np -> Np -> N
sub_neg = curry $ \ case
    (XI x, XI y) -> un_suivi_de (sub_neg x y)
    (XI x, XO y) -> zero_suivi_de (sub_pos x y)
    (XI x, XH)   -> Pos (double_moins_un x)
    (XO x, XI y) -> zero_suivi_de (sub_neg x y)
    (XO x, XO y) -> un_suivi_de (sub_neg x y)
    (XO x, XH)   -> double_moins_deux x
    (XH,   XI y) -> Pos (XO y)
    (XH,   XO y) -> Pos (double_moins_un y)
    (XH,   XH)   -> Nul

sub' :: N -> N -> Maybe N
sub' a Nul = Just a
sub' Nul _ = Nothing
sub' (Pos a) (Pos b) = case Np.sub' a b of
    GT' c -> Just (Pos c)
    EQ' -> Just Nul
    _ -> Nothing

top_more_informative :: N -> N -> N -> N -> Maybe (N, N)
top_more_informative a b c d = [(x, y) | x <- sub' a c, y <- sub' b d, any (> Nul) [x, y]]

quadratic_top_more_informative :: N -> N -> N -> N -> N -> N -> N -> N -> Maybe (N, N, N, N)
quadratic_top_more_informative a b c d e f g h =
    [(w, x, y, z) | w <- sub' a e, x <- sub' b f, y <- sub' c g, z <- sub' d h
                  , any (> Nul) [w, x, y, z]]
