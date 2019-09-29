module Main where

import Test.SmallCheck
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.SmallCheck

import Data.Q
import Data.Z

main :: IO ()
main = defaultMain $ testGroup ""
    [testGroup "Z"
     [testProperty "addition" $ \ a b ->
      a + b == fromZ (toZ a + toZ b),
      testProperty "subtraction" $ \ a b ->
      a - b == fromZ (toZ a - toZ b),
      testProperty "multiplication" $ \ a b ->
      a * b == fromZ (toZ a * toZ b)
     ],
     testGroup "Qpos"
     [testProperty "conversion" $ \ (Positive a) ->
      a == fromQpos (toQpos a),
      testProperty "comparison" $ \ (Positive a) (Positive b) ->
      compare a b == compare (toQpos a) (toQpos b)
     ],
     testGroup "Q"
     [testProperty "conversion" $ \ a ->
      a == fromQ (toQ a),
      testProperty "comparison" $ \ a b ->
      compare a b == compare (toQ a) (toQ b),
      testProperty "addition" $ \ a b ->
      a + b == fromQ (toQ a + toQ b),
      testProperty "subtraction" $ \ a b ->
      a - b == fromQ (toQ a - toQ b),
      testProperty "multiplication" $ \ a b ->
      a * b == fromQ (toQ a * toQ b),
      testProperty "division" $ \ a b ->
      0 /= b ==>
      a / b == fromQ (toQ a / toQ b)
     ]
    ]
