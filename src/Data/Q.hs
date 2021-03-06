{-# LANGUAGE RebindableSyntax, ImplicitPrelude #-}
module Data.Q where

import "base" Prelude (abs, fromRational, fromIntegral, negate, signum, toRational)
import qualified "base" Prelude as Base
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (Read (..))
import Text.Show (Show (..))

import qualified Data.Ratio as Base

import Data.Difference
import Data.N  as N
import Data.Np as Np
import Data.Z  as Z

data Qp = NR Qp
        | DL Qp
        | One
  deriving (Base.Eq)

instance PartialEq Qp where (≡) = (Base.==)
instance Preord Qp where x ≤ y = GT ≢ compare x y
instance Eq Qp
instance PartialOrd Qp where tryCompare x y = Just (compare x y)
instance Ord Qp where
    compare x y = case qquadratic_Qp_to_Q 0 (1) (-1) 0 0 0 0 1 x y of
        Zero -> EQ
        Qpos _ -> GT
        Qneg _ -> LT
instance Base.Ord Qp where compare = compare

instance {-# OVERLAPPING #-} Semigroup (Sum Qp) where
    Sum x <> Sum y = Sum (qquadratic' 0 1 1 0 0 0 0 1 x y)

data Q = Zero
       | Qpos Qp
       | Qneg Qp
  deriving (Base.Eq)

instance PartialEq Q where (≡) = (Base.==)
instance Preord Q where x ≤ y = GT ≢ compare x y
instance Eq Q
instance PartialOrd Q where tryCompare x y = Just (compare x y)
instance Ord Q where
    compare = curry $ \ case
        (Zero, Zero) -> EQ
        (Zero, Qpos _) -> LT
        (Zero, Qneg _) -> GT
        (Qneg _, Qpos _) -> LT
        (Qpos p, Qpos q) -> compare p q
        (Qneg p, Qneg q) -> compare q p
        (p, q) -> compare EQ $ compare q p
instance Base.Ord Q where compare = compare

instance Base.Num Q where
    (+) = qquadratic 0 1 1 0 0 0 0 1
    (-) = qquadratic 0 (1) (-1) 0 0 0 0 1
    (*) = qquadratic 1 0 0 0 0 0 0 1
    abs (Qneg qpos) = Qpos qpos
    abs q = q
    signum Zero = 0
    signum (Qpos _) = 1
    signum (Qneg _) = negate 1
    fromInteger = fromRational . Base.fromInteger

instance Base.Fractional Q where
    (/) = qquadratic 0 1 0 0 0 0 1 0
    fromRational q = fraction_encoding (fromIntegral $ Base.numerator q) (fromIntegral $ Base.denominator q)

instance FromInteger Q where fromInteger = Base.fromInteger

toQ :: Base.Rational -> Q
toQ = fromRational

fromQ :: Q -> Base.Rational
fromQ Zero = 0
fromQ (Qpos q) = id     . toRational $ fromQpos q
fromQ (Qneg q) = negate . toRational $ fromQpos q

toQpos :: Base.Ratio Natural -> Qp
toQpos q = positive_fraction_encoding (toZ $ Base.numerator q) (toZ $ Base.denominator q)

fromQpos :: Qp -> Base.Ratio Natural
fromQpos One = 1
fromQpos (NR q) = 1 + fromQpos q
fromQpos (DL q) = 1 / (1 + 1 / fromQpos q)

positive_fraction_encoding' :: Np -> Np -> Qp
positive_fraction_encoding' x y = case Np.sub' x y of
    EQ' -> One
    LT' z -> DL (positive_fraction_encoding' x z)
    GT' z -> DL (positive_fraction_encoding' z y)

positive_fraction_encoding :: Z -> Z -> Qp
positive_fraction_encoding x y = case compare x y of
    EQ -> One
    LT -> DL (positive_fraction_encoding x (y - x))
    GT -> NR (positive_fraction_encoding (x - y) y)

fraction_encoding' :: Z -> Np -> Q
fraction_encoding' ZERO _ = Zero
fraction_encoding' (POS n) d = Qpos (positive_fraction_encoding' n d)
fraction_encoding' (NEG n) d = Qneg (positive_fraction_encoding' n d)

fraction_encoding :: Z -> Z -> Q
fraction_encoding m n = case m * n of
    ZERO -> Zero
    POS _ -> Qpos qpos
    NEG _ -> Qneg qpos
  where qpos = positive_fraction_encoding (abs m) (abs n)

qhomographic_sign
 :: Z -> Z -> Z -> Z -> Qp
 -> (Z, ((Z, Z, Z, Z), Qp))
qhomographic_sign a b c d p = case (p, o) of
    (NR q, ZERO) -> qhomographic_sign a (a + b) c (c + d) q
    (DL q, ZERO) -> qhomographic_sign (a + b) b (c + d) d q
    _ -> (signum o1 * signum o2, ((a, b, c, d), p))
  where
    o1 = outside [a, b]
    o2 = outside [c, d]
    o = bool 1 o1 (0 ≢ b) * bool 1 o2 (0 ≢ d)

qhomographic_Qp_to_Q :: Z -> Z -> Z -> Z -> Qp -> Q
qhomographic_Qp_to_Q a b c d p
  | a * d ≡ b * c = case d of
        ZERO -> fraction_encoding a c
        _    -> fraction_encoding b d
  | otherwise = case s' of
        ZERO -> Zero
        POS _ -> Qpos q
        NEG _ -> Qneg q
  where
    (s', ((a', b', c', d'), p')) = qhomographic_sign a b c d p
    q = qhomographic' (zabs a') (zabs b') (zabs c') (zabs d') p'

qhomographic :: Z -> Z -> Z -> Z -> Q -> Q
qhomographic a b c d = \ case
    Zero -> fraction_encoding b d
    Qpos p -> qhomographic_Qp_to_Q a b c d p
    Qneg p -> qhomographic_Qp_to_Q (negate a) b (negate c) d p

qhomographic' :: N -> N -> N -> N -> Qp -> Qp
qhomographic' a b c d
  | a * d ≡ b * c = pure $ case d of
    Nul -> positive_fraction_encoding (fromN a) (fromN c)
    _   -> positive_fraction_encoding (fromN b) (fromN d)
  | otherwise = \ case
    One -> positive_fraction_encoding (fromN $ a + b) (fromN $ c + d)
    p | Just (x, y) <- N.top_more_informative a b c d -> NR (qhomographic' x y c d p)
      | Just (x, y) <- N.top_more_informative c d a b -> DL (qhomographic' a b x y p)
    NR q -> qhomographic' a (a + b) c (c + d) q
    DL q -> qhomographic' (a + b) b (c + d) d q

qquadratic_sign
 :: Z -> Z -> Z -> Z -> Z -> Z -> Z -> Z
 -> Qp -> Qp
 -> (Z, ((Z, Z, Z, Z, Z, Z, Z, Z), (Qp, Qp)))
qquadratic_sign a b c d e f g h p q = case (p, q, o') of
    (_, One, _) ->
        let (s', ((a', b', c', d'), r')) = qhomographic_sign (a + b) (c + d) (e + f) (g + h) p
        in (s', ((ZERO, a', ZERO, b', ZERO, c', ZERO, d'), (r', One)))
    (One, _, _) ->
        let (s', ((a', b', c', d'), r')) = qhomographic_sign (a + c) (b + d) (e + g) (f + h) q
        in (s', ((ZERO, ZERO, a', b', ZERO, ZERO, c', d'), (One, r')))
    (NR xs, NR ys, ZERO) -> qquadratic_sign
        a (a + b) (a + c) (a + b + c + d) e (e + f) (e + g) (e + f + g + h) xs ys
    (NR xs, DL ys, ZERO) -> qquadratic_sign
        (a + b) b (a + b + c + d) (b + d) (e + f) f (e + f + g + h) (f + h) xs ys
    (DL xs, NR ys, ZERO) -> qquadratic_sign
        (a + c) (a + b + c + d) c (c + d) (e + g) (e + f + g + h) g (g + h) xs ys
    (DL xs, DL ys, ZERO) -> qquadratic_sign
        (a + b + c + d) (b + d) (c + d) d (e + f + g + h) (f + h) (g + h) h xs ys
    _ -> (signum o1 * signum o2, ((a, b, c, d, e, f, g, h), (p, q)))
  where
    o1 = outside [a, b, c, d]
    o2 = outside [e, f, g, h]
    o1' = Base.iterate ((-) <*> signum) o1 Base.!! 2
    o2' = Base.iterate ((-) <*> signum) o2 Base.!! 2
    o' = bool 1 o1' ((0, 0, 0) ≢ (b, c, d)) * bool 1 o2' ((0, 0, 0) ≢ (f, g, h))

qquadratic_Qp_to_Q :: Z -> Z -> Z -> Z -> Z -> Z -> Z -> Z -> Qp -> Qp -> Q
qquadratic_Qp_to_Q a b c d e f g h p q
  | same_ratio_dec_inf a b c d e f g h = case (e, f, g) of
        (ZERO, ZERO, ZERO) -> fraction_encoding d h
        (ZERO, ZERO, _)    -> fraction_encoding c g
        (ZERO, _,    _)    -> fraction_encoding b f
        (_,    _,    _)    -> fraction_encoding a e
  | otherwise = case s' of
         ZERO -> Zero
         POS _ -> Qpos r
         NEG _ -> Qneg r
  where
    (s', ((a', b', c', d', e', f', g', h'), (p', q'))) = qquadratic_sign a b c d e f g h p q
    r = qquadratic' (zabs a') (zabs b') (zabs c') (zabs d') (zabs e') (zabs f') (zabs g') (zabs h') p' q'

qquadratic :: Z -> Z -> Z -> Z -> Z -> Z -> Z -> Z -> Q -> Q -> Q
qquadratic a b c d e f g h = curry $ \ case
    (Zero, s2) -> qhomographic c d g h s2
    (s1, Zero) -> qhomographic b d f h s1
    (Qpos p, Qpos q) -> blah id id p q
    (Qpos p, Qneg q) -> blah id negate p q
    (Qneg p, Qpos q) -> blah negate id p q
    (Qneg p, Qneg q) -> blah negate negate p q
  where blah φ χ = qquadratic_Qp_to_Q ((φ . χ) a) (φ b) (χ c) d ((φ . χ) e) (φ f) (χ g) h

qquadratic' :: N -> N -> N -> N -> N -> N -> N -> N -> Qp -> Qp -> Qp
qquadratic' a b c d e f g h
  | same_ratio_dec_inf a b c d e f g h = curry . pure $ case (e, f, g) of
    (Nul, Nul, Nul) -> positive_fraction_encoding (fromN d) (fromN h)
    (Nul, Nul, _)   -> positive_fraction_encoding (fromN c) (fromN g)
    (Nul, _,   _)   -> positive_fraction_encoding (fromN b) (fromN f)
    (_,   _,   _)   -> positive_fraction_encoding (fromN a) (fromN e)
  | otherwise = curry $ \ case
    (One, q) -> qhomographic' (a + c) (b + d) (e + g) (f + h) q
    (p, One) -> qhomographic' (a + b) (c + d) (e + f) (g + h) p
    (p, q)
      | Just (w, x, y, z) <- N.quadratic_top_more_informative a b c d e f g h -> NR
        (qquadratic' w x y z e f g h p q)
      | Just (w, x, y, z) <- N.quadratic_top_more_informative e f g h a b c d -> DL
        (qquadratic' a b c d w x y z p q)
    (NR xs, NR ys) -> qquadratic'
        a (a + b) (a + c) (a + b + c + d) e (e + f) (e + g) (e + f + g + h) xs ys
    (NR xs, DL ys) -> qquadratic'
        (a + b) b (a + b + c + d) (b + d) (e + f) f (e + f + g + h) (f + h) xs ys
    (DL xs, NR ys) -> qquadratic'
        (a + c) (a + b + c + d) c (c + d) (e + g) (e + f + g + h) g (g + h) xs ys
    (DL xs, DL ys) -> qquadratic'
        (a + b + c + d) (b + d) (c + d) d (e + f + g + h) (f + h) (g + h) h xs ys

same_ratio_dec_inf
 :: (Base.Eq a, Semigroup (Product a)) => a -> a -> a -> a -> a -> a -> a -> a -> Bool
same_ratio_dec_inf a b c d e f g h =
  (a * f, b * g, c * h, a * g, a * h, b * h) Base.==
  (b * e, c * f, d * g, c * e, d * e, d * f)

instance Show Qp where
    show = \ case
        One -> ""
        DL p' -> 'L':show p'
        NR p' -> 'R':show p'

instance Show Q where
    show = \ case
        Zero -> "0"
        Qpos p' -> show p'
        Qneg p' -> '-':show p'

instance Read Qp where
    readPrec = ReadPrec.lift $
        foldr id One <$> many (DL <$ ReadP.char 'L' <|> NR <$ ReadP.char 'R')

instance Read Q where
    readPrec = ReadPrec.readP_to_Prec $ \ prec ->
        Zero <$ ReadP.char '0' <|>
        fromMaybe Qpos <$> optional (Qneg <$ ReadP.char '-' <|> Qpos <$ ReadP.char '+')
                       <*> ReadPrec.readPrec_to_P readPrec prec
