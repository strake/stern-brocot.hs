module Data.Positive where

import Numeric.Natural

data Positive = XI Positive
              | XO Positive
              | XH

add :: Positive -> Positive -> Positive
add x y =
  case x of
    XI x' ->
      (case y of
         XI y' -> XO (add_carry x' y')
         XO y' -> XI (add x' y')
         XH -> XO (add_un x'))
    XO x' ->
      (case y of
         XI y' -> XI (add x' y')
         XO y' -> XO (add x' y')
         XH -> XI x')
    XH ->
      (case y of
         XI y' -> XO (add_un y')
         XO y' -> XI y'
         XH -> XO XH)

add_carry :: Positive -> Positive -> Positive
add_carry x y =
  case x of
    XI x' ->
      (case y of
         XI y' -> XI (add_carry x' y')
         XO y' -> XO (add_carry x' y')
         XH -> XI (add_un x'))
    XO x' ->
      (case y of
         XI y' -> XO (add_carry x' y')
         XO y' -> XI (add x' y')
         XH -> XO (add_un x'))
    XH ->
      (case y of
         XI y' -> XI (add_un y')
         XO y' -> XO (add_un y')
         XH -> XI XH)

double_moins_un :: Positive -> Positive
double_moins_un x =
  case x of
    XI x' -> XI (XO x')
    XO x' -> XI (double_moins_un x')
    XH -> XH

ccompare :: Positive -> Positive -> Ordering -> Ordering
ccompare x y r =
  case x of
    XI x' ->
      (case y of
         XI y' -> ccompare x' y' r
         XO y' -> ccompare x' y' GT
         XH -> GT)
    XO x' ->
      (case y of
         XI y' -> ccompare x' y' LT
         XO y' -> ccompare x' y' r
         XH -> GT)
    XH -> (case y of
             XI _ -> LT
             XO _ -> LT
             XH -> r)

times :: Positive -> Positive -> Positive
times x y = case x of
    XI x' -> add y (XO (times x' y))
    XO x' -> XO (times x' y)
    XH -> y

add_un :: Positive -> Positive
add_un = \ case
    XI x' -> XO (add_un x')
    XO x' -> XI x'
    XH -> XO XH

nattoPos :: Integral a => a -> Positive
nattoPos x
  | 1 == x       = XH
  | 1 == mod x 2 = XI (nattoPos (div x 2))
  | otherwise    = XO (nattoPos (div x 2))

fromPos :: Positive -> Natural
fromPos XH = 1
fromPos (XI n) = 2*fromPos n + 1
fromPos (XO n) = 2*fromPos n
