module Data.Z where

import Data.Foldable (foldMap)
import Data.Monoid (Sum (..))

import Data.Np

data Z = ZERO
       | POS Np
       | NEG Np
  deriving (Eq)

instance Num Z where
    ZERO + y = y
    x + ZERO = x
    POS x + POS y = POS (add x y)
    POS x + NEG y = case sub' x y of
        EQ' -> ZERO
        LT' z -> NEG z
        GT' z -> POS z
    NEG x + POS y = case sub' x y of
        EQ' -> ZERO
        LT' z -> POS z
        GT' z -> NEG z
    NEG x + NEG y = NEG (add x y)
    ZERO * _ = ZERO
    _ * ZERO = ZERO
    POS x * POS y = POS (times x y)
    POS x * NEG y = NEG (times x y)
    NEG x * POS y = NEG (times x y)
    NEG x * NEG y = POS (times x y)
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

instance Ord Z where
    compare ZERO ZERO = EQ
    compare ZERO (POS _) = LT
    compare ZERO (NEG _) = GT
    compare (POS x) (POS y) = ccompare x y EQ
    compare (POS _) _ = GT
    compare (NEG x) (NEG y) = ccompare y x EQ
    compare (NEG _) _ = LT

toZ :: Integral a => a -> Z
toZ x = case compare x 0 of
    EQ -> ZERO
    GT -> POS (nattoPos x)
    LT -> NEG (nattoPos (negate x))

fromZ :: Z -> Integer
fromZ ZERO = 0
fromZ (POS n) = id     . fromIntegral $ fromPos n
fromZ (NEG n) = negate . fromIntegral $ fromPos n

top_more_informative :: Z -> Z -> Z -> Z -> Bool
top_more_informative a b c d = a >= c && b >= d && (a > c || b > d)

quadratic_top_more_informative, same_ratio_dec_inf :: Z -> Z -> Z -> Z -> Z -> Z -> Z -> Z -> Bool
quadratic_top_more_informative a b c d e f g h =
  e <= a && f <= b && g <= c && h <= d && (e < a || f < b || g < c || h < d)
same_ratio_dec_inf a b c d e f g h =
  (a * f, b * g, c * h, a * g, a * h, b * h) ==
  (b * e, c * f, d * g, c * e, d * e, d * f)

outside :: Foldable f => f Z -> Z
outside = getSum . foldMap (Sum . signum)
