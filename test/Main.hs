module Main where

import Test.SmallCheck
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.SmallCheck

import Data.Z
import Quadratic

main :: IO ()
main = defaultMain $ testGroup ""
    [testGroup "Z"
     [testProperty "addition" $ \ a b ->
      a + b == fromZ (toZ a + toZ b)
     ],
     testGroup "Qpos"
     [testProperty "conversion" $ \ (Positive a) ->
      a == fromQpos (toQpos a)
     ],
     testGroup "Q"
     [testProperty "conversion" $ \ a ->
      a == fromQ (toQ a),
      testProperty "addition" $ \ a b ->
      a + b == fromQ (toQ a `qplus` toQ b),
      testProperty "subtraction" $ \ a b ->
      a - b == fromQ (toQ a `qminus` toQ b),
      testProperty "multiplication" $ \ a b ->
      a * b == fromQ (toQ a `qmult` toQ b),
      testProperty "division" $ \ a b ->
      0 /= b ==>
      a / b == fromQ (toQ a `qdiv` toQ b)
     ]
    ]
