module Data.N where

import Data.Np
import Data.Z

data N = Nul
       | Pos Np
  deriving (Eq, Ord)

eadd :: N -> N -> N
eadd Nul y = y
eadd x Nul = x
eadd (Pos x) (Pos y) = Pos (add x y)

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

zabs :: Z -> N
zabs = \ case
    ZERO -> Nul
    POS p -> Pos p
    NEG p -> Pos p
