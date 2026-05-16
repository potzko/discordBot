{-# LANGUAGE OverloadedStrings #-}
module Bot.NumberProperties.Perfect (property, isPerfect) where

import Bot.NumberProperties.Types (Property, simpleProperty)
import Bot.NumberProperties.Internal (properDivisorSum)

isPerfect :: Integer -> Bool
isPerfect n = n >= 2 && properDivisorSum n == n

-- | Divisor-sum is O(sqrt n); past 10^12 it gets slow enough to threaten the
--   per-message timeout. Adjust if you have a faster divisor-sum.
maxN :: Integer
maxN = 10 ^ (12 :: Int)

property :: Property
property = simpleProperty "perfect" (<= maxN) isPerfect
