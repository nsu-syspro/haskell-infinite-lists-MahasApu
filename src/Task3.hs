{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
-- The above pragma enables all warnings

module Task3 where

import Task2 (Stream, Stream(..), fromList)
import Data.Ratio (Ratio, numerator)

-- | Power series represented as infinite stream of coefficients
-- 
-- For following series
--   @a0 + a1 * x + a2 * x^2 + ...@
-- coefficients would be
--   @a0, a1, a2, ...@
--
-- Usage examples:
--
-- >>> coefficients (x + x ^ 2 + x ^ 4)
-- [0,1,1,0,1,0,0,0,0,0]
-- >>> coefficients ((1 + x)^5)
-- [1,5,10,10,5,1,0,0,0,0]
-- >>> coefficients (42 :: Series Integer)
-- [42,0,0,0,0,0,0,0,0,0]
--
newtype Series a = Series
  { coefficients :: Stream a
  -- ^ Returns coefficients of given power series
  --
  -- For following series
  --   @a0 + a1 * x + a2 * x^2 + ...@
  -- coefficients would be
  --   @a0, a1, a2, ...@
  }

instance Show s => Show (Series s) where
  show :: Show s => Series s -> String
  show (Series s) = show s

instance Functor Series where
  fmap :: (a -> b) -> Series a -> Series b
  fmap f (Series s) = Series $ fmap f s

instance Applicative Series where
  pure :: a -> Series a
  pure s = Series $ pure s

  (<*>) :: Series (a -> b) -> Series a -> Series b
  Series fs <*> Series xs = Series $ fs <*> xs


-- >>> Series (fromList 0 [1,2,3,4]) * Series (fromList 0 [1,2,3,4,5])
-- [1,4,10,20,30,34,31,20,0,0]
-- >>> x^2
-- [0,0,1,0,0,0,0,0,0,0]

instance Num a => Num (Series a) where
  (+) :: Num a => Series a -> Series a -> Series a
  (+) s1 s2 = (+) <$> s1 <*> s2

  (*) :: Num a => Series a -> Series a -> Series a
  (*) (Series s1) (Series s2) = Series $ s1 * s2

  signum :: Num a => Series a -> Series a
  signum = (<$>) signum

  fromInteger :: Num a => Integer -> Series a
  fromInteger = Series . fromInteger

  negate :: Num a => Series a -> Series a
  negate = (<$>) negate

  abs :: Num a => Series a -> Series a
  abs    = (<$>) abs


instance Fractional a => Fractional (Series a) where
  fromRational :: Fractional a => Rational -> Series a
  fromRational = Series . fromRational

  (/) :: Fractional a => Series a -> Series a -> Series a
  (/) (Series s1) (Series s2) = Series $ s1 / s2

-- | Power series corresponding to single @x@
--
-- First 10 coefficients:
--
-- >>> coefficients x
-- [0,1,0,0,0,0,0,0,0,0]
--
x :: Num a => Series a
x = Series $ fromList 0 [0, 1]


-- | Multiplies power series by given number
-- 
-- For following series
--   @a0 + a1 * x + a2 * x^2 + ...@
-- coefficients would be
--   @a0, a1, a2, ...@
--
-- Usage examples:
--
-- >>> coefficients (2 *: (x + x ^ 2 + x ^ 4))
-- [0,2,2,0,2,0,0,0,0,0]
-- >>> coefficients (2 *: ((1 + x)^5))
-- [2,10,20,20,10,2,0,0,0,0]
--

infixl 7 *:
(*:) :: Num a => a -> Series a -> Series a
(*:) a  = fmap (* a)

-- | Helper function for producing integer
-- coefficients from generating function
-- (assuming denominator of 1 in all coefficients)
--
-- Usage example:
--
-- >>> gen $ (2 + 3 * x)
-- [2,3,0,0,0,0,0,0,0,0]
--
gen :: Series (Ratio Integer) -> Stream Integer
gen (Series s) = numerator <$> s

-- | Returns infinite stream of ones
--
-- First 10 elements:
--
-- >>> ones
-- [1,1,1,1,1,1,1,1,1,1]
--
ones :: Stream Integer
ones = gen $ 1 / (1 - x)

-- | Returns infinite stream of natural numbers (excluding zero)
--
-- First 10 natural numbers:
-- 
-- >>> nats
-- [1,2,3,4,5,6,7,8,9,10]
--
nats :: Stream Integer
nats = gen $ 1 / (1 - 2 * x + x * x)

-- | Returns infinite stream of fibonacci numbers (starting with zero)
--
-- First 10 fibonacci numbers:
--
-- >>> fibs
-- [0,1,1,2,3,5,8,13,21,34]
--
fibs :: Stream Integer
fibs = gen $ x / (1 - x - x * x)

