module Data.Z where

import Control.Monad (guard)

import Data.Entier
import Data.Positive

data Z = ZERO
       | POS Positive
       | NEG Positive

instance Num Z where
    (+) = zplus
    (-) = zminus
    (*) = zmult
    abs = zabs
    signum = zsgn
    fromInteger = toZ

zplus :: Z -> Z -> Z
zplus x y = case x of
    ZERO -> y
    POS x' ->
      (case y of
         ZERO -> x
         POS y' -> POS (add x' y')
         NEG y' ->
           (case ccompare x' y' EQ of
              EQ -> ZERO
              LT -> NEG (true_sub y' x')
              GT -> POS (true_sub x' y')))
    NEG x' ->
      (case y of
         ZERO -> x
         POS y' ->
           (case ccompare x' y' EQ of
              EQ -> ZERO
              LT -> POS (true_sub y' x')
              GT -> NEG (true_sub x' y'))
         NEG y' -> NEG (add x' y'))
  where
    true_sub x0 y0 = case sub_pos x0 y0 of
        Nul -> XH
        Pos z -> z

zopp :: Z -> Z
zopp = \ case
    ZERO -> ZERO
    POS x0 -> NEG x0
    NEG x0 -> POS x0

zmult :: Z -> Z -> Z
zmult x y =
  case x of
    ZERO -> ZERO
    POS x' ->
      (case y of
         ZERO -> ZERO
         POS y' -> POS (times x' y')
         NEG y' -> NEG (times x' y'))
    NEG x' ->
      (case y of
         ZERO -> ZERO
         POS y' -> NEG (times x' y')
         NEG y' -> POS (times x' y'))

instance Eq Z where
    x == y = EQ == compare x y

instance Ord Z where
    compare x y = case x of
        ZERO -> case y of
            ZERO -> EQ
            POS _ -> LT
            NEG _ -> GT
        POS x' -> case y of
            ZERO -> GT
            POS y' -> ccompare x' y' EQ
            NEG _ -> GT
        NEG x' -> case y of
            ZERO -> LT
            POS _ -> LT
            NEG y' -> op (ccompare x' y' EQ)

zsgn :: Z -> Z
zsgn = \ case
    ZERO -> ZERO
    POS _ -> POS XH
    NEG _ -> NEG XH

zabs :: Z -> Z
zabs = \ case
    ZERO -> ZERO
    POS p -> POS p
    NEG p -> POS p

zminus :: Z -> Z -> Z
zminus m n = zplus m (zopp n)

zccompare_rec :: Z -> Z -> (() -> a) -> (() -> a) -> (() -> a) -> a
zccompare_rec x y h1 h2 h3 = case compare x y of
    EQ -> h1 ()
    LT -> h2 ()
    GT -> h3 ()

z_eq_dec, z_lt_dec, z_le_dec, z_le_lt_eq_dec :: Z -> Z -> Bool
z_eq_dec x y = zccompare_rec x y (\_ -> True) (\_ -> False) (\_ -> False)
z_lt_dec x y = zccompare_rec x y (\_ -> False) (\_ -> True) (\_ -> False)
z_le_dec x y = zccompare_rec x y (\_ -> True) (\_ -> True) (\_ -> False)
z_le_lt_eq_dec x y = zccompare_rec x y (\_ -> False) (\_ -> True) (\_ -> undefined)

z_zerop :: Z -> Bool
z_zerop = (== ZERO)

outside_interval :: Z -> Z -> Z
outside_interval a b = zplus (zsgn a) (zsgn b)

inside_interval_1_dec_inf, inside_interval_2_dec_inf :: Z -> Z -> Bool

inside_interval_1_dec_inf a b = case (compare ZERO a, compare ZERO b) of
    (LT, LT) -> True
    (GT, GT) -> True
    _        -> False

inside_interval_2_dec_inf a b = case (compare ZERO a, compare ZERO b) of
    (LT, GT) -> True
    (GT, LT) -> True
    _        -> False

op :: Ordering -> Ordering
op = \ case
    EQ -> EQ
    LT -> GT
    GT -> LT

toZ :: Integral a => a -> Z
toZ x = case compare x 0 of
    EQ -> ZERO
    GT -> POS (nattoPos x)
    LT -> NEG (nattoPos (negate x))

fromZ :: Z -> Integer
fromZ ZERO = 0
fromZ (POS n) = fromIntegral $ fromPos n
fromZ (NEG n) = negate . fromIntegral $ fromPos n

not_Zeq_inf :: Z -> Z -> Bool
not_Zeq_inf x y =
  case z_lt_dec x y of
    True -> True
    False ->
      (case z_le_lt_eq_dec y x of
         True -> False
         False -> undefined)

z_dec :: Z -> Z -> Maybe Bool
z_dec x y =
  case z_lt_dec x y of
    True -> Just True
    False ->
      (case z_le_lt_eq_dec y x of
         True -> Just False
         False -> Nothing)

z_dec' :: Z -> Z -> Maybe Bool
z_dec' x y = not_Zeq_inf x y <$ guard (x /= y)

quadro_leq_inf :: Z -> Z -> Z -> Z -> Bool
quadro_leq_inf a b c d =
  case z_lt_dec a c of
    True -> False
    False -> (case z_lt_dec b d of
                True -> False
                False -> True)

zsgn_1 :: Z -> Maybe Bool
zsgn_1 x =
  case x of
    ZERO -> Just True
    POS _ -> Just False
    NEG _ -> Nothing

top_more_informative :: Z -> Z -> Z -> Z -> Bool
top_more_informative a b c d =
  case quadro_leq_inf a b c d of
    True ->
        case z_le_lt_eq_dec c a of
          True -> True
          False -> z_le_lt_eq_dec d b
    False -> False

outside_square :: Z -> Z -> Z -> Z -> Z
outside_square a b c d =
  zplus (zplus (zplus (zsgn a) (zsgn b)) (zsgn c)) (zsgn d)

three_integers_dec_inf :: Z -> Z -> Z -> Bool
three_integers_dec_inf a b c =
  case z_zerop a of
    True -> (case z_zerop b of
               True -> z_zerop c
               False -> False)
    False -> False

inside_square_1_dec_inf, inside_square_2_dec_inf :: Z -> Z -> Bool

inside_square_1_dec_inf o1 o2 =
  case z_lt_dec (POS (XO XH)) o1 of
    True -> z_lt_dec (POS (XO XH)) o2
    False ->
      (case z_lt_dec o1 (NEG (XO XH)) of
         True -> z_lt_dec o2 (NEG (XO XH))
         False -> False)

inside_square_2_dec_inf o1 o2 =
  case z_lt_dec (POS (XO XH)) o1 of
    True -> z_lt_dec o2 (NEG (XO XH))
    False ->
      (case z_lt_dec o1 (NEG (XO XH)) of
         True -> z_lt_dec (POS (XO XH)) o2
         False -> False)
