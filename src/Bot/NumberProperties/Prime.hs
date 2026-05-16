{-# LANGUAGE OverloadedStrings #-}
module Bot.NumberProperties.Prime (property) where

import Bot.NumberProperties.Types (Property, simpleProperty)
import Bot.Primality (isProbablyPrime)

property :: Property
property = simpleProperty "prime" (const True) isProbablyPrime
