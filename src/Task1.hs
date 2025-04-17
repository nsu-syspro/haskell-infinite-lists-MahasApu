{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

import Data.List (unfoldr)
import Prelude hiding(iterate)

-- | Some experiments with unfoldr
-- >>> take 10 $ iterate (+1) 10
-- [10,11,12,13,14,15,16,17,18,19]
--
iterate :: (a -> a) -> a -> [a]
iterate f = unfoldr (\x  -> Just (x, f x))

-- | Returns infinite list of natural numbers (excluding zero)
--
-- First 10 natural numbers:
--
-- >>> take 10 nats
-- [1,2,3,4,5,6,7,8,9,10]
--
nats :: [Integer]
nats = unfoldr (\x -> Just (x, x + 1)) 1

 
-- | Returns infinite list of fibonacci numbers (starting with zero)
--
-- First 10 fibonacci numbers:
--
-- >>> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]
--
fibs :: [Integer]
fibs = unfoldr fibStep (0, 1) 
    where 
        fibStep (x, y) = Just (x, (y, x + y))


-- | Returns infinite list of prime numbers
--
-- First 10 prime numbers:
--
-- >>> take 10 primes
-- [2,3,5,7,11,13,17,19,23,29]
--
primes :: [Integer]
primes = unfoldr sieve [2..]

-- | One step of Sieve of Eratosthenes
-- (to be used with 'unfoldr')
--
-- Returns next prime number from given list
-- and strikes out all multiples of this prime
-- from the rest of the list
--
-- Usage example:
--
-- >>> sieve [2..20]
-- Just (2,[3,5,7,9,11,13,15,17,19])
-- >>> sieve [3,5..20]
-- Just (3,[5,7,11,13,17,19])
--
sieve :: [Integer] -> Maybe (Integer, [Integer])
sieve [ ]      = Nothing 
sieve (x : xs) = Just (x, sieveStep x xs)
    where 
        sieveStep s ps = [p | p <- ps, p `mod` s /= 0]
