{-|
Module      : Bot.Primality
Description : Probabilistic primality test (Miller-Rabin with small-prime prefilter).
              Used by Bot.NumberPropertyDetector and any other module that needs primality.
-}

{-# LANGUAGE OverloadedStrings #-}
module Bot.Primality (isProbablyPrime) where

import qualified Data.IntSet as IntSet
import System.Random (randomRs, mkStdGen)

-- | Hybrid primality test for arbitrary-size integers.
--   Small numbers hit a precomputed sieve; larger numbers go through a
--   small-prime prefilter, a Fermat base-2 check, then Miller-Rabin.
isProbablyPrime :: Integer -> Bool
isProbablyPrime n
  | n < 2                        = False
  | n < 10000                    = IntSet.member (fromIntegral n) smallPrimes
  | divBySmallPrimes n           = False
  | not (fermatBase2 n)          = False
  | not (rabinMillerWitness n 3) = False
  | otherwise = all (rabinMillerWitness n)
                    (take 5 $ generateRandomWitnesses n 5)

fermatBase2 :: Integer -> Bool
fermatBase2 n = modPow 2 (n - 1) n == 1

divBySmallPrimes :: Integer -> Bool
divBySmallPrimes n = any (\p -> n `mod` p == 0) [3, 5, 7, 11, 13, 17, 19]

generateRandomWitnesses :: Integer -> Int -> [Integer]
generateRandomWitnesses n k
  | n <= 3    = []
  | otherwise = take k $ randomRs (2, n - 2) (mkStdGen (fromIntegral n))

rabinMillerWitness :: Integer -> Integer -> Bool
rabinMillerWitness n a
  | gcd a n > 1 = False
  | otherwise =
      let (d, s) = decompose n
          x = modPow a d n
      in x == 1
         || x == n - 1
         || any (\r -> modPow a (d * 2 ^ r) n == n - 1) [1 .. s - 1]

-- | Decompose n-1 as d * 2^s where d is odd.
decompose :: Integer -> (Integer, Int)
decompose n = go (n - 1) 0
  where
    go d s
      | odd d     = (d, s)
      | otherwise = go (d `div` 2) (s + 1)

-- | Modular exponentiation: a^b mod m.
modPow :: Integer -> Integer -> Integer -> Integer
modPow a b m = go a b 1
  where
    go _ 0 result = result
    go base expn result
      | odd expn  = go (base * base `mod` m) (expn `div` 2) (result * base `mod` m)
      | otherwise = go (base * base `mod` m) (expn `div` 2) result

smallPrimes :: IntSet.IntSet
smallPrimes = IntSet.fromAscList $ sieve 10000
  where
    sieve limit = 2 : sieve' [3, 5 .. limit]
    sieve' []     = []
    sieve' (p:xs) = p : sieve' [x | x <- xs, x `mod` p /= 0]
