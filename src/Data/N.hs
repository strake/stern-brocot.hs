module Data.N where

import Data.Semigroup (Semigroup (..), Sum (..))

import Data.Np hiding (sub')
import qualified Data.Np as Np

data N = Nul
       | Pos Np
  deriving (Eq, Ord)

instance {-# OVERLAPPING #-} Semigroup (Sum N) where
    Sum Nul <> y = y
    x <> Sum Nul = x
    Sum (Pos x) <> Sum (Pos y) = Pos <$> Sum x <> Sum y

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
    XI x' -> Pos (XO (XO x'))
    XO x' -> Pos (XO (double_moins_un x'))
    XH -> Nul

sub_pos :: Np -> Np -> N
sub_pos x y =
  case x of
    XI x' ->
      case y of
        XI y' -> zero_suivi_de (sub_pos x' y')
        XO y' -> un_suivi_de (sub_pos x' y')
        XH -> Pos (XO x')
    XO x' ->
      case y of
        XI y' -> un_suivi_de (sub_neg x' y')
        XO y' -> zero_suivi_de (sub_pos x' y')
        XH -> Pos (double_moins_un x')
    XH ->
      case y of
        XI y' -> Pos (double_moins_un y')
        XO y' -> double_moins_deux y'
        XH -> Nul

sub_neg :: Np -> Np -> N
sub_neg x y =
  case x of
    XI x' ->
      case y of
        XI y' -> un_suivi_de (sub_neg x' y')
        XO y' -> zero_suivi_de (sub_pos x' y')
        XH -> Pos (double_moins_un x')
    XO x' ->
      case y of
        XI y' -> zero_suivi_de (sub_neg x' y')
        XO y' -> un_suivi_de (sub_neg x' y')
        XH -> double_moins_deux x'
    XH ->
      case y of
        XI y' -> Pos (XO y')
        XO y' -> Pos (double_moins_un y')
        XH -> Nul

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
