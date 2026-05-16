{-# LANGUAGE OverloadedStrings #-}
module Bot.NumberProperties.Triangular (property, isTriangular) where

import Bot.NumberProperties.Types (Property, simpleProperty)
import Bot.NumberProperties.Internal (isPerfectSquare)

-- | n is triangular iff 8n + 1 is a perfect square.
--   Equivalent to: n = k(k+1)/2 for some non-negative integer k, with
--   k = (sqrt(8n+1) - 1) / 2.
isTriangular :: Integer -> Bool
isTriangular n
  | n < 0     = False
  | otherwise = isPerfectSquare (8 * n + 1)

property :: Property
property = simpleProperty "triangular" (const True) isTriangular
