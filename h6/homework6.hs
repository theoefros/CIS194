{-# LANGUAGE FlexibleInstances #-}

-- A recursive function to compute fibonacci numbers
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- And an infite list of fibonacci numbers using fib
fibs1 :: [Integer]
fibs1 = map fib [0..]

-- A more efficient wat to compute fibonacci numbers
-- Takes ~100 seconds to compurte 1000000-th fibonacci number...
-- ~1.4 secons to copmute the 100 000th fibonacci number...
fib2 :: Integer -> Integer
fib2 = go 0 1
    where go i j 0 = i
          go i j 1 = j
          go i j n = go j (i+j) (n-1)
-- A more efficient way of computing fibonacci numbers
fibs2 :: [Integer]
fibs2 = map fib2 [0..]

-- And some lazyness
-- This one doesn't start with 0...
fibs3 = 1 : zipWith (+) (0 : fibs3) fibs3

fibs4 = 0 : 1 : zipWith (+) fibs4 (tail fibs4)

-- A data type for streams
data Stream a = Stream a (Stream a)

-- Show only first 20 elements of stream
instance Show a => Show (Stream a) where
    show a = show $ take 20 $ streamToList a
-- Convert a Stream to list
streamToList :: Stream a -> [a]
streamToList (Stream a stream) = a : streamToList stream 

-- Generate a repeating stream of given element
streamRepeat :: a -> Stream a
streamRepeat a = Stream a (streamRepeat a)

-- Map a function over stream
streamMap :: (a -> b) -> Stream a -> Stream b 
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs) 

-- Generates a Stream from seed of type a, which is the first element, and an
-- unfolding rule of type a -> a
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Stream seed (streamFromSeed f (f seed))

-- An infinite Stream of natural numbers 0,1,2,...
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- A Stream which corresponds to the ruler funcition where the n-th element in
-- the Stream is the largest power of 2 which evenly divides n (n starts with 1) 
ruler :: Stream Integer
ruler = go 0
    where go n = interleaveStreams (streamRepeat n) (go (n + 1))

-- Alternates the elements from two streams
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) ys = Stream x (interleaveStreams ys xs)

-- Computing Fibonacci numbers using Streams... magic...
-- Define x, x = 0 + 1x + 0x^2 + 0x^3 + . . . .
x :: Stream Integer
x = Stream 0 (streamFromSeed (\n -> 0) 1)

instance Num (Stream Integer) where
    fromInteger n = Stream n (streamRepeat 0)
    negate (Stream x xs)  = Stream (-x) (negate xs)
    (Stream x xs) + (Stream y ys) = Stream (x+y) (xs + ys)
    (Stream x xs) * b@(Stream y ys) = Stream (x*y) ((streamMap (*x) ys) + (xs * b))
    
instance Fractional (Stream Integer) where
    (Stream x xs) / (Stream y ys) = q
        where q = Stream (x `div` y) (streamMap (`div` y) (xs - q*ys))
-- magic
fibs5 :: Stream Integer
fibs5 = x / (1 - x - x^2)

-- Computing Fibonacci numbers using matrices
-- There's some mathemagic involved...
-- Consult the homework pdf for more details (06-laziness.pdf)
data Matrix x1 x2 y1 y2 = Matrix Integer Integer Integer Integer

instance Show (Matrix a b c d) where
    show (Matrix x1 x2 y1 y2) = "[" ++  show x1 ++ " " ++ show x2 ++ "\n" ++ 
                                show y1 ++ " " ++ show y2  ++ "]"

instance Num (Matrix a b c d) where
    -- Matrix multiplication
    (Matrix a11 a12 a21 a22) * (Matrix b11 b12 b21 b22) = Matrix ab11 ab12 ab21 ab22  
        where ab11 = a11 * b11 + a12 * b21
              ab12 = a11 * b12 + a12 * b22
              ab21 = a21 * b11 + a22 * b21
              ab22 = a21 * b12 + a22 * b22

-- Matrix used to compute fibonacci numbers
f :: Matrix Integer Integer Integer Integer
f = Matrix 1 1 1 0
-- This computes the 1000000-th fibonacci number in less than a second...
-- it's mathemagic...
fib6 :: Integer -> Integer
fib6 0 = 0
fib6 n = let Matrix a b c d = f^n in b