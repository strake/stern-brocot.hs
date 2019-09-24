module Data.Np (Np (..), toNp, fromNp, sub', double_moins_un) where

import qualified "base" Prelude as Base

import Data.Difference

data Np = XI Np
        | XO Np
        | XH
  deriving (Base.Eq)

instance PartialEq Np where (≡) = (Base.==)
instance Preord Np where (≤) = (Base.<=)
instance Eq Np
instance PartialOrd Np where tryCompare x y = Just (compare x y)
instance Ord Np where compare = Base.compare

instance {-# OVERLAPPING #-} Semigroup (Sum Np) where
    Sum x <> Sum y = Sum (add x y)

instance {-# OVERLAPPING #-} Semigroup (Product Np) where
    Product x <> Product y = Product (times x y)

instance {-# OVERLAPPING #-} Monoid (Product Np) where
    mempty = Product XH

add :: Np -> Np -> Np
add = go False where
    go c = curry $ \ case
        (XI x, XI y) -> bool XO XI c (go True  x y)
        (XI x, XO y) -> bool XI XO c (go c     x y)
        (XI x, XH)   -> bool XO XI c (add_un x)
        (XO x, XO y) -> bool XO XI c (go False x y)
        (XO x, XH)
          | not c    -> XI x
          | True     -> XO (add_un x)
        (XH,   XH)   -> bool XO XI c XH
        (x,    y)    -> go c y x

double_moins_un :: Np -> Np
double_moins_un = \ case
    XI x -> XI (XO x)
    XO x -> XI (double_moins_un x)
    XH -> XH

instance Base.Ord Np where
    compare = go EQ where
        go r = curry $ \ case
            (XH, XH) -> r
            (XH, _)  -> LT
            (_,  XH) -> GT
            (XI x, XI y) -> go r  x y
            (XI x, XO y) -> go GT x y
            (XO x, XI y) -> go LT x y
            (XO x, XO y) -> go r  x y

times :: Np -> Np -> Np
times x y = case x of
    XI x -> XO (times x y) + y
    XO x -> XO (times x y)
    XH -> y

add_un :: Np -> Np
add_un = \ case
    XI x -> XO (add_un x)
    XO x -> XI x
    XH -> XO XH

toNp :: Base.Integral a => a -> Np
toNp 1 = XH
toNp x = bool XO XI (0 Base./= Base.mod x 2) (toNp (Base.div x 2))

fromNp :: Np -> Natural
fromNp XH = 1
fromNp (XI n) = 2*fromNp n + 1
fromNp (XO n) = 2*fromNp n

sub' :: Np -> Np -> Difference Np
sub' (XI x) (XI y) = XO <$> sub' x y
sub' (XI x) (XO y) = case sub' x y of
    LT' z -> LT' (double_moins_un z)
    EQ'   -> GT' XH
    GT' z -> GT' (XI z)
sub' (XO x) (XI y) = case sub' x y of
    LT' z -> LT' (XI z)
    EQ'   -> LT' XH
    GT' z -> GT' (double_moins_un z)
sub' (XO x) (XO y) = XO <$> sub' x y
sub' x XH = maybe EQ' GT' $ sub_un x
sub' XH y = maybe EQ' LT' $ sub_un y

sub_un :: Np -> Maybe Np
sub_un XH = Nothing
sub_un (XI n) = Just (XO n)
sub_un (XO XH) = Just XH
sub_un (XO n) = XI <$> sub_un n
