module Bot.NumberProperties.Internal
  ( properDivisorSum
  , isqrt
  , isPerfectSquare
  ) where

-- | Sum of proper divisors of n (excludes n itself). O(sqrt n).
--   Shared by Perfect and Abundant.
properDivisorSum :: Integer -> Integer
properDivisorSum n
  | n < 2     = 0
  | otherwise = go 2 1
  where
    go i acc
      | i * i > n      = acc
      | i * i == n     = acc + i
      | n `mod` i == 0 = go (i + 1) (acc + i + n `div` i)
      | otherwise      = go (i + 1) acc

-- | Integer square root: floor(sqrt n) for n >= 0. Newton's method.
--   Converges in O(log n) iterations from the initial guess of n.
isqrt :: Integer -> Integer
isqrt n
  | n < 0     = error "isqrt: negative input"
  | n < 2     = n
  | otherwise = go n
  where
    go x =
      let x' = (x + n `div` x) `div` 2
      in if x' >= x then x else go x'

-- | True iff n is a non-negative perfect square.
isPerfectSquare :: Integer -> Bool
isPerfectSquare n
  | n < 0     = False
  | otherwise = let s = isqrt n in s * s == n
