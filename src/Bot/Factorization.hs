{-|
Module      : Bot.Factorization
Description : Integer factorization via Pollard rho-Brent + Miller-Rabin.
              Pure functions; the Discord-facing wrapper lives in Bot.Factorize.
-}

{-# LANGUAGE BangPatterns #-}
module Bot.Factorization (factorize, pollardBrent) where

import Data.List (group, sort)
import Bot.Primality (isProbablyPrime)

-- | Pollard's rho with Brent's improvement.
--   Returns a non-trivial factor 1 < d < n, or Nothing if every retry produced n itself.
--   Caller should already have ruled out primality and small factors.
pollardBrent :: Integer -> Maybe Integer
pollardBrent n
  | n < 4     = Nothing
  | even n    = Just 2
  | otherwise = tryC 1
  where
    f !c !x = (x * x + c) `mod` n
    m = 128 :: Integer  -- batch size between gcds

    tryC !c
      | c > 30    = Nothing  -- give up after enough distinct polynomials
      | otherwise = case run c of
          Just d  -> Just d
          Nothing -> tryC (c + 1)

    run !c = phase1 2 1 1
      where
        applyN 0 !x = x
        applyN k !x = applyN (k - 1) (f c x)

        batched 0 !y !q _  = (y, q)
        batched k !y !q !x =
          let !y' = f c y
              !q' = (q * abs (x - y')) `mod` n
          in batched (k - 1) y' q' x

        -- Doubling outer loop. Whenever gcd hits n we fall back to phase2
        -- to find the actual factor that the batch missed.
        phase1 !y !r !q =
          let !x       = y
              !yPrime  = applyN r y
              (!yEnd, !qEnd, !g, !ys) = inner yPrime q 0 r x
          in if g == 1
               then phase1 yEnd (r * 2) qEnd
               else if g == n
                      then phase2 ys x
                      else Just g

        inner !y !q !k !r !x
          | k >= r    = (y, q, gcd q n, y)
          | gNew > 1  = (yNew, qNew, gNew, y)  -- ys = y at start of this batch
          | otherwise = inner yNew qNew (k + steps) r x
          where
            !steps         = min m (r - k)
            (!yNew, !qNew) = batched steps y q x
            !gNew          = gcd qNew n

        -- Replay the failing batch one step at a time to recover the smaller gcd.
        phase2 !ys0 !x = step ys0 (m + 16)
          where
            step !_ 0  = Nothing
            step !y !k =
              let !yN = f c y
                  !g' = gcd (abs (x - yN)) n
              in if g' == n  then Nothing
                 else if g' > 1 then Just g'
                 else step yN (k - 1)

-- | Full prime factorization, sorted by base.
--   Strategy: strip every prime up to `trialBound` by trial division
--   (regardless of how large n is, so 2^100 * p * q peels off the 2's
--   before the polynomial ever runs), then hand any composite remainder
--   to Miller-Rabin + Pollard rho-Brent.
factorize :: Integer -> [(Integer, Int)]
factorize n
  | n < 2     = []
  | otherwise = collect (allPrimeFactors n)

collect :: [Integer] -> [(Integer, Int)]
collect xs = [(p, length g) | g@(p:_) <- group (sort xs)]

-- | Trial-division upper bound. Any prime factor < this is stripped here;
--   anything bigger is left for Pollard rho.
trialBound :: Integer
trialBound = 10000

allPrimeFactors :: Integer -> [Integer]
allPrimeFactors = stripFrom 2

-- | Try `d` as a divisor of `n`. Strip all copies, advance to the next odd
--   candidate, and stop trial-dividing once `d` passes `trialBound`.
stripFrom :: Integer -> Integer -> [Integer]
stripFrom d n
  | n < 2          = []
  | n `mod` d == 0 = d : stripFrom d (n `div` d)
  | d * d > n      = [n]                 -- remainder is prime
  | d > trialBound = rhoSplit n          -- hand off to Pollard rho-Brent
  | otherwise      = stripFrom (nextCandidate d) n
  where
    nextCandidate 2 = 3
    nextCandidate k = k + 2

-- | Factor n using Miller-Rabin + Pollard rho-Brent. Caller has already
--   stripped all primes below `trialBound`, so any factor returned here
--   should itself be larger than `trialBound`.
rhoSplit :: Integer -> [Integer]
rhoSplit n
  | n < 2             = []
  | isProbablyPrime n = [n]
  | otherwise = case pollardBrent n of
      Just d  -> rhoSplit d ++ rhoSplit (n `div` d)
      Nothing -> [n]  -- algorithmic give-up; treat as prime
