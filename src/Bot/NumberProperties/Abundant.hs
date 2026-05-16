{-# LANGUAGE OverloadedStrings #-}
module Bot.NumberProperties.Abundant (property, isAbundant) where

import Bot.NumberProperties.Types (Property, simpleProperty)
import Bot.NumberProperties.Internal (properDivisorSum)

isAbundant :: Integer -> Bool
isAbundant n = n >= 2 && properDivisorSum n > n

-- | Same O(sqrt n) cost profile as Perfect; share the bound.
maxN :: Integer
maxN = 10 ^ (12 :: Int)

property :: Property
property = simpleProperty "abundant" (<= maxN) isAbundant
