{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}
-- The above pragma enables all warnings

module Task2 where
import Data.Foldable (Foldable(toList))
import Prelude hiding(iterate)
import Task1(iterate)

-- | Infinite stream of elements
data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
  show :: Show a => Stream a -> String
  show s = show $ take 10 $ toList s

instance Foldable Stream where
  foldMap :: Monoid m => (a -> m) -> Stream a -> m
  foldMap f (Stream a s) = f a <> foldMap f s

-- >>> fmap (* 2) $ fromList undefined [1..]
-- [2,4,6,8,10,12,14,16,18,20]
-- >>> fromList 2 []
-- [2,2,2,2,2,2,2,2,2,2]
--
instance Functor Stream where
  fmap :: (a -> b) -> Stream a -> Stream b
  fmap f (Stream a s) = Stream (f a) (fmap f s)


instance Applicative Stream where
  pure :: a -> Stream a
  pure x = Stream x $ pure x
  (<*>) :: Stream (a -> b) -> Stream a -> Stream b
  Stream f fs <*> Stream x xs = Stream (f x) (fs <*> xs)

-- >>> 2 *:: fromList undefined [1..]
-- [2,4,6,8,10,12,14,16,18,20]
--
infixl 7 *::
(*::) :: Num a => a -> Stream a -> Stream a
(*::) a  = fmap (* a) 


-- >>> :t fromInteger
-- fromInteger :: Num a => Integer -> a
-- >>> :t fromList 0 . (: [])
-- fromList 0 . (: []) :: Num a => a -> Stream a
-- >>> :t fromList 0 . pure
-- fromList 0 . pure :: Num a => a -> Stream a
--
instance Num a => Num (Stream a) where
  (+) :: Num a => Stream a -> Stream a -> Stream a
  (+) s1 s2 = (+) <$> s1 <*> s2

  (*) :: Num a => Stream a -> Stream a -> Stream a
  (*) (Stream x xs) s@(Stream y ys) = Stream (x * y) (x *:: ys + xs * s)

  signum :: Num a => Stream a -> Stream a
  signum = fmap signum

  fromInteger :: Num a => Integer -> Stream a
  fromInteger =  fromList 0 . pure . fromInteger

  negate :: Num a => Stream a -> Stream a
  negate = fmap negate
  
  abs :: Num a => Stream a -> Stream a
  abs = fmap abs


instance Fractional a => Fractional (Stream a) where
  fromRational :: Fractional a => Rational -> Stream a
  fromRational = fromList 0 . pure . fromRational

  (/) :: Fractional a => Stream a -> Stream a -> Stream a
  (/) (Stream x xs) s@(Stream y ys) = Stream factor rational 
    where
      factor = x / y
      rational = (xs - factor *:: ys) / s

  
-- | Converts given list into stream
--
-- If the list is finite then it is continued
-- with given value repeated infinitely
--
-- Usage example:
--
-- >>> fromList 0 [1,2,3]
-- [1,2,3,0,0,0,0,0,0,0]
-- >>> fromList undefined [1..]
-- [1,2,3,4,5,6,7,8,9,10]
--
fromList :: a -> [a] -> Stream a
fromList x (y : ys) = Stream y $ fromList x ys
fromList x []       = Stream x $ fromList x []

-- | Builds stream from given seed value by applying given step function
--
-- Step function produces a pair of the next element in stream and updated seed value.
--
-- Usage example:
--
-- >>> unfold (\x -> (x, x-1)) 5
-- [5,4,3,2,1,0,-1,-2,-3,-4]
-- >>> unfold (\x -> (abs x, x-1)) 5
-- [5,4,3,2,1,0,1,2,3,4]
--
unfold :: (b -> (a, b)) -> b -> Stream a
unfold f b = let seed@(a, _) = f b
             in  fromList a (fst <$> iterate (f . snd) seed)
  

-- | Returns infinite stream of natural numbers (excluding zero)
--
-- First 10 natural numbers:
--
-- >>> nats
-- [1,2,3,4,5,6,7,8,9,10]
--
nats :: Stream Integer
nats = unfold (\x -> (x, x + 1)) 1

-- | Returns infinite stream of fibonacci numbers (starting with zero)
--
-- First 10 fibonacci numbers:
--
-- >>> fibs
-- [0,1,1,2,3,5,8,13,21,34]
--
fibs :: Stream Integer
fibs = unfold fibStep (0, 1) 
    where 
        fibStep (x, y) = (x, (y, x + y))


-- | Returns infinite stream of prime numbers
--
-- First 10 prime numbers:
--
-- >>> primes
-- [2,3,5,7,11,13,17,19,23,29]
--
primes :: Stream Integer
primes = unfold sieve $ fromList 2 [2..]

-- | One step of Sieve of Eratosthenes
-- (to be used with 'unfoldr')
--
-- Returns next prime number from given stream
-- and strikes out all multiples of this prime
-- from the rest of the stream
--
-- Usage example:
--
-- >>> sieve $ fromList 0 [2..]
-- (2,[3,5,7,9,11,13,15,17,19,21])
-- >>> sieve $ snd $ sieve $ fromList 0 [2..]
-- (3,[5,7,11,13,17,19,23,25,29,31])
--
sieve :: Stream Integer -> (Integer, Stream Integer)
sieve (Stream a s) = (a, sieveStep a s)
  where
    sieveStep x (Stream y ys)
      | y `mod` x /= 0 = Stream y (sieveStep x ys)
      | otherwise      = sieveStep x ys

