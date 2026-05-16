module Bot.NumberProperties.Types
  ( Property(..)
  , simpleProperty
  ) where

import qualified Data.Text as T

-- | A named property over integers.
--
--   `propertyApplies n` is a fast guard saying "this property is tractable
--   for n" — used to skip slow checks (e.g. O(sqrt n) divisor-sum) on huge
--   inputs so they don't starve the cheap ones. Must be constant-time-ish.
--
--   `propertyLabel n` returns `Just label` when the property holds for n
--   (and Nothing otherwise). Returning the label here — rather than just a
--   Bool — lets a property include extra info: e.g. Fibonacci returns
--   `Just "fibonacci(123)"` instead of just `Just "fibonacci"`.
--
--   For plain yes/no properties, build via `simpleProperty`.
data Property = Property
  { propertyName    :: T.Text
  , propertyApplies :: Integer -> Bool
  , propertyLabel   :: Integer -> Maybe T.Text
  }

-- | Build a property whose label is just `name` whenever the predicate holds.
--   Use this for plain yes/no properties; build the Property record directly
--   if you want extra info in the label (see Fibonacci for an index example).
simpleProperty :: T.Text -> (Integer -> Bool) -> (Integer -> Bool) -> Property
simpleProperty name applies check = Property
  { propertyName    = name
  , propertyApplies = applies
  , propertyLabel   = \n -> if check n then Just name else Nothing
  }
