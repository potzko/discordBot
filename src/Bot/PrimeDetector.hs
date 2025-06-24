{-|
Module      : Bot.PrimeDetector
Description : Detects prime numbers in messages and replies with whether they are prime or not.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Bot.PrimeDetector (primeAction) where

import Discord.Types
import Control.Monad (void, unless)
import qualified Data.Text as T
import Bot.Types
import System.Random (randomRs, mkStdGen)
import qualified Data.IntSet as IntSet
import qualified Data.Text.Read as TR
import Data.Maybe (mapMaybe)
import qualified Data.Cache.LRU as LRU
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.IO.Class (liftIO)
import System.Timeout (timeout)
import Bot.Util (sendMessageSafe)

-- Global LRU cache for isPrime results (last 100)
{-# NOINLINE primeCacheRef #-}
primeCacheRef :: IORef (LRU.LRU Integer Bool)
primeCacheRef = unsafePerformIO (newIORef (LRU.newLRU (Just 100)))

-- | Hybrid primality test for large numbers
isProbablyPrime :: Integer -> Bool
isProbablyPrime n
  | n < 2 = False
  | n < 10000 = IntSet.member (fromIntegral n) smallPrimes
  | divBySmallPrimes n = False
  | not (fermatBase2 n) = False
  | not (rabinMillerWitness n 3) = False
  | otherwise = all (rabinMillerWitness n) (take 5 $ generateRandomWitnesses n 5)

-- | Fermat base-2 prefilter
fermatBase2 :: Integer -> Bool
fermatBase2 n = modPow 2 (n-1) n == 1

-- | Cached isPrime check now uses isProbablyPrime
cachedIsPrime :: Integer -> IO Bool
cachedIsPrime n = atomicModifyIORef' primeCacheRef $ \cache ->
  case LRU.lookup n cache of
    (cache', Just result) -> (cache', result)
    (cache', Nothing) -> let result = isProbablyPrime n
                             cache'' = LRU.insert n result cache'
                         in (cache'', result)

-- | The passive prime number detection action.
--   When a message contains numbers, the bot checks if any are prime and replies with the results.
primeAction :: BotAction GlobalState
primeAction = BotAction
  { botActionName = "PrimeDetector"
  , matchMsg = \_ msg -> unsafePerformIO $ do
      let nums = extractNumbers msg
      anyM cachedIsPrime nums
  , runAction = \event _ -> case event of
      MessageCreate msg -> unless (userIsBot (messageAuthor msg)) do
        let txt = messageContent msg
        let numbers = extractNumbers txt
        result <- liftIO $ timeout (5000000 :: Int) (filterM (cachedIsPrime) numbers)
        case result of
          Nothing -> return ()
          Just primeNumbers -> case primeNumbers of
            [] -> return ()
            _ -> do
              let formatted = T.unlines [formatPrime n | n <- primeNumbers]
              void $ sendMessageSafe "PrimeDetector" (messageChannelId msg) formatted
      _ -> return ()
  }

-- | Monadic any
anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = return False
anyM p (x:xs) = do
  q <- p x
  if q then return True else anyM p xs

-- | Monadic filter
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM p (x:xs) = do
  q <- p x
  ys <- filterM p xs
  return (if q then x:ys else ys)

-- | Extracts all numbers from a text message (fast, safe)
extractNumbers :: T.Text -> [Integer]
extractNumbers = mapMaybe (either (const Nothing) (Just . fst) . TR.decimal) . T.words

-- | Pre-filter: check divisibility by small primes
--   (skip 2 since even numbers are already filtered)
divBySmallPrimes :: Integer -> Bool
divBySmallPrimes n = any (\p -> n `mod` p == 0) [3, 5, 7, 11, 13, 17, 19]

-- | Generate k random witnesses between 2 and n-2 (no nub, no uniqueness needed)
generateRandomWitnesses :: Integer -> Int -> [Integer]
generateRandomWitnesses n k
  | n <= 3 = []
  | otherwise = let gen = mkStdGen (fromIntegral n)
                in take k $ randomRs (2, n-2) gen

-- | Single round of Rabin-Miller test
rabinMillerWitness :: Integer -> Integer -> Bool
rabinMillerWitness n a
  | gcd a n > 1 = False  -- n is composite
  | otherwise = 
      let (d, s) = decompose n
          x = modPow a d n
      in x == 1 || x == n - 1 || any (\r -> modPow a (d * 2^r) n == n - 1) [1..s-1]

-- | Decompose n-1 as d * 2^s where d is odd
decompose :: Integer -> (Integer, Int)
decompose n = decompose' (n - 1) 0
  where decompose' d s
          | odd d = (d, s)
          | otherwise = decompose' (d `div` 2) (s + 1)

-- | Modular exponentiation: a^b mod m
modPow :: Integer -> Integer -> Integer -> Integer
modPow a b m = modPow' a b m 1
  where modPow' _ 0 _ result = result
        modPow' base expn modulus result
          | odd expn = modPow' (base * base `mod` modulus) (expn `div` 2) modulus (result * base `mod` modulus)
          | otherwise = modPow' (base * base `mod` modulus) (expn `div` 2) modulus result

-- | Formats the prime number result
formatPrime :: Integer -> T.Text
formatPrime n = T.pack (show n ++ " is prime! ✨")

smallPrimes :: IntSet.IntSet
smallPrimes = IntSet.fromAscList $ sieve 10000
  where
    sieve limit = 2 : sieve' [3,5..limit]
    sieve' [] = []
    sieve' (p:xs) = p : sieve' [x | x <- xs, x `mod` p /= 0]
