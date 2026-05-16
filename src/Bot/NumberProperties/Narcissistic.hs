{-# LANGUAGE OverloadedStrings #-}
module Bot.NumberProperties.Narcissistic (property, isNarcissistic) where

import Bot.NumberProperties.Types (Property, simpleProperty)

-- | Narcissistic (Armstrong) number: n equals the sum of its digits each
--   raised to the power of the digit count. e.g. 153 = 1^3 + 5^3 + 3^3.
isNarcissistic :: Integer -> Bool
isNarcissistic n
  | n < 0     = False
  | otherwise =
      let ds = digits n
          k  = length ds
      in sum [toInteger d ^ k | d <- ds] == n

-- | Decimal digits of n (most-significant first). digits 0 = [0].
digits :: Integer -> [Int]
digits 0 = [0]
digits n = go [] n
  where
    go acc 0 = acc
    go acc k = let (q, r) = k `quotRem` 10
               in go (fromInteger r : acc) q

property :: Property
property = simpleProperty "narcissistic" (const True) isNarcissistic
