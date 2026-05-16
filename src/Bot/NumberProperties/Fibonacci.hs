{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Bot.NumberProperties.Fibonacci (property, fibonacciIndex) where

import qualified Data.Text as T
import Data.Bits (shiftR)
import Bot.NumberProperties.Types (Property(..))

-- | Index of n in the Fibonacci sequence (F_0 = 0, F_1 = 1, F_2 = 1, ...).
--   Returns Nothing if n is not a Fibonacci number.
--
--   Strategy:
--     1. Estimate the index via Binet:  k ~ (log2 n + log2 sqrt 5) / log2 phi.
--        We use the bit-length of n as a stand-in for log2 n, so this works
--        even when n is way past Double's range.
--     2. Compute F_k via O(log k) fast doubling.
--     3. Take a couple of corrective steps (walk up or down) to land exactly.
--   For F_123, that's ~7 multiplies + 0 walks, vs ~123 additions in the old
--   linear walk. The gap widens fast for larger indices.
fibonacciIndex :: Integer -> Maybe Int
fibonacciIndex n
  | n <  0 = Nothing
  | n == 0 = Just 0
  | n == 1 = Just 1
  | otherwise =
      let kEst       = max 2 (estimateK n)
          (fk, fk1)  = fibPair kEst
      in adjust kEst fk fk1
  where
    adjust !k !fk !fk1
      | fk == n  = Just k
      | fk <  n  = walkUp   (k + 1) fk1          (fk + fk1)
      | otherwise = walkDown (k - 1) (fk1 - fk)  fk

    walkUp !k !fk !fk1
      | fk == n   = Just k
      | fk >  n   = Nothing
      | otherwise = walkUp (k + 1) fk1 (fk + fk1)

    -- Invariant: fk = F_k, fk1 = F_(k+1). Step down via F_(k-1) = F_(k+1) - F_k.
    walkDown !k !fk !fk1
      | fk == n   = Just k
      | fk <  n   = Nothing
      | otherwise = walkDown (k - 1) (fk1 - fk) fk

-- | Fast doubling: returns (F_k, F_(k+1)) in O(log k) Integer multiplications.
fibPair :: Int -> (Integer, Integer)
fibPair 0 = (0, 1)
fibPair k =
  let (a, b) = fibPair (k `div` 2)
      c = a * (2 * b - a)
      d = a * a + b * b
  in if even k then (c, d) else (d, c + d)

-- | Approximate index of n in Fibonacci via Binet's formula.
--   Uses bit-length so the floating-point arithmetic stays in range no
--   matter how large n grows.
estimateK :: Integer -> Int
estimateK n =
  let log2N = fromIntegral (bitLength n - 1) :: Double
  in round ((log2N + log2Sqrt5) / log2Phi)
  where
    log2Phi   = 0.6942419136306174   -- log2 ((1 + sqrt 5) / 2)
    log2Sqrt5 = 1.1609640474436814   -- log2 (sqrt 5)

-- | Number of bits needed to represent n. bitLength 0 = 0, bitLength 1 = 1.
bitLength :: Integer -> Int
bitLength = go 0
  where
    go !i 0  = i
    go !i n  = go (i + 1) (n `shiftR` 1)

property :: Property
property = Property
  { propertyName    = "fibonacci"
  , propertyApplies = const True
  , propertyLabel   = \n -> case fibonacciIndex n of
      Just k  -> Just (T.pack ("fibonacci(" ++ show k ++ ")"))
      Nothing -> Nothing
  }
