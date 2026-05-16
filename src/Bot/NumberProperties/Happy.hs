{-# LANGUAGE OverloadedStrings #-}
module Bot.NumberProperties.Happy (property, isHappy) where

import Bot.NumberProperties.Types (Property, simpleProperty)

-- | Happy: iterating digit-square-sum eventually reaches 1.
--   Unhappy numbers fall into the cycle containing 4, which gives us a
--   constant-time termination check.
isHappy :: Integer -> Bool
isHappy n
  | n < 1     = False
  | otherwise = go n
  where
    go 1 = True
    go 4 = False  -- 4 -> 16 -> 37 -> 58 -> 89 -> 145 -> 42 -> 20 -> 4
    go k = go (digitSquareSum k)

digitSquareSum :: Integer -> Integer
digitSquareSum = go 0
  where
    go acc 0 = acc
    go acc n = let (q, r) = n `quotRem` 10 in go (acc + r * r) q

property :: Property
property = simpleProperty "happy" (const True) isHappy
