module Data.Z where

import "base" Prelude as Base (Num (negate, signum))
import qualified "base" Prelude as Base

import Data.Difference
import Data.N hiding (sub')
import Data.Np

data Z = ZERO
       | POS Np
       | NEG Np
  deriving (Base.Eq)

instance Base.Num Z where
    ZERO + y = y
    x + ZERO = x
    POS x + POS y = POS (x + y)
    POS x + NEG y = case sub' x y of
        EQ' -> ZERO
        LT' z -> NEG z
        GT' z -> POS z
    NEG x + POS y = case sub' x y of
        EQ' -> ZERO
        LT' z -> POS z
        GT' z -> NEG z
    NEG x + NEG y = NEG (x + y)
    ZERO * _ = ZERO
    _ * ZERO = ZERO
    POS x * POS y = POS (x * y)
    POS x * NEG y = NEG (x * y)
    NEG x * POS y = NEG (x * y)
    NEG x * NEG y = POS (x * y)
    abs = \ case
        ZERO -> ZERO
        POS p -> POS p
        NEG p -> POS p
    signum = \ case
        ZERO -> ZERO
        POS _ -> POS XH
        NEG _ -> NEG XH
    negate = \ case
        ZERO -> ZERO
        POS p -> NEG p
        NEG p -> POS p
    fromInteger = toZ

instance Base.Ord Z where
    compare ZERO ZERO = EQ
    compare ZERO (POS _) = LT
    compare ZERO (NEG _) = GT
    compare (POS x) (POS y) = compare x y
    compare (POS _) _ = GT
    compare (NEG x) (NEG y) = compare y x
    compare (NEG _) _ = LT

instance PartialEq Z where (≡) = (Base.==)
instance Eq Z
instance Preord Z where (≤) = (Base.<=)
instance PartialOrd Z where tryCompare x y = Just (compare x y)
instance Ord Z where compare = Base.compare

toZ :: Base.Integral a => a -> Z
toZ x = case Base.compare x 0 of
    EQ -> ZERO
    GT -> POS (toNp x)
    LT -> NEG (toNp (negate x))

fromZ :: Z -> Base.Integer
fromZ ZERO = 0
fromZ (POS n) = id     . Base.fromIntegral $ fromNp n
fromZ (NEG n) = negate . Base.fromIntegral $ fromNp n

same_ratio_dec_inf :: Z -> Z -> Z -> Z -> Z -> Z -> Z -> Z -> Bool
same_ratio_dec_inf a b c d e f g h =
  (a * f, b * g, c * h, a * g, a * h, b * h) Base.==
  (b * e, c * f, d * g, c * e, d * e, d * f)

outside :: Foldable f => f Z -> Z
outside = getSum . foldMap (Sum . signum)

zabs :: Z -> N
zabs = \ case
    ZERO -> Nul
    POS p -> Pos p
    NEG p -> Pos p

fromN :: N -> Z
fromN Nul = ZERO
fromN (Pos x) = POS x
