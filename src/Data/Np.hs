module Data.Np where

import Numeric.Natural

data Np = XI Np
        | XO Np
        | XH
  deriving (Eq)

instance Ord Np where
    compare x y = ccompare x y EQ

add :: Np -> Np -> Np
add x y =
  case x of
    XI x' ->
      case y of
        XI y' -> XO (add_carry x' y')
        XO y' -> XI (add x' y')
        XH -> XO (add_un x')
    XO x' ->
      case y of
        XI y' -> XI (add x' y')
        XO y' -> XO (add x' y')
        XH -> XI x'
    XH ->
      case y of
        XI y' -> XO (add_un y')
        XO y' -> XI y'
        XH -> XO XH

add_carry :: Np -> Np -> Np
add_carry x y =
  case x of
    XI x' ->
      case y of
        XI y' -> XI (add_carry x' y')
        XO y' -> XO (add_carry x' y')
        XH -> XI (add_un x')
    XO x' ->
      case y of
        XI y' -> XO (add_carry x' y')
        XO y' -> XI (add x' y')
        XH -> XO (add_un x')
    XH ->
      case y of
        XI y' -> XI (add_un y')
        XO y' -> XO (add_un y')
        XH -> XI XH

double_moins_un :: Np -> Np
double_moins_un x =
  case x of
    XI x' -> XI (XO x')
    XO x' -> XI (double_moins_un x')
    XH -> XH

ccompare :: Np -> Np -> Ordering -> Ordering
ccompare x y r =
  case x of
    XI x' ->
      case y of
        XI y' -> ccompare x' y' r
        XO y' -> ccompare x' y' GT
        XH -> GT
    XO x' ->
      case y of
        XI y' -> ccompare x' y' LT
        XO y' -> ccompare x' y' r
        XH -> GT
    XH -> case y of
            XI _ -> LT
            XO _ -> LT
            XH -> r

times :: Np -> Np -> Np
times x y = case x of
    XI x' -> add y (XO (times x' y))
    XO x' -> XO (times x' y)
    XH -> y

add_un :: Np -> Np
add_un = \ case
    XI x' -> XO (add_un x')
    XO x' -> XI x'
    XH -> XO XH

nattoPos :: Integral a => a -> Np
nattoPos x
  | 1 == x       = XH
  | 1 == mod x 2 = XI (nattoPos (div x 2))
  | otherwise    = XO (nattoPos (div x 2))

fromPos :: Np -> Natural
fromPos XH = 1
fromPos (XI n) = 2*fromPos n + 1
fromPos (XO n) = 2*fromPos n

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

data Difference a = LT' a | EQ' | GT' a
  deriving (Functor, Eq, Show)
