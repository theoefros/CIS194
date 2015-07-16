{-
http://www.cis.upenn.edu/~cis194/hw/01-intro.pdf

Validation algorithm for credit cards. 

It follows these steps:
  Double the value of every second digit beginning from the right.
  That is, the last digit is unchanged; the second-to-last digit is dou-
  bled; the third-to-last digit is unchanged; and so on. For example,
  [1,3,8,6] becomes [2,3,16,6]

  Add the digits of the doubled values and the undoubled digits from the original 
  number. For example, [2,3,16,6] becomes 2+3+1+6+6 = 18

  Calculate the remainder when the sum is divided by 10. For the above example, 
  the remainder would be 8. If the result equals 0, then the number is valid.

-}
import Data.Char

-- A function to convert positive Integers to a list of digits.
-- For invalid inputs (negative numbers, 0) it returns []
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

-- Same function, reverse order of output
toDigitsRev = reverse . toDigits

-- Doubles every other number beginning from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs
    | even (length zipped) = map (\(e, n) -> if odd n then e else e*2) zipped
    | otherwise = map (\(e, n) -> if odd n then e*2 else e) zipped
    where zipped = zip xs [0..]

doubleEveryOther' xs = foldr double [] zipped
    where zipped = zip xs [l, (l - 1)..1]
          l = length xs
          double (n, i) acc = if even i then n*2:acc else n:acc

-- Sum the digits of a list of integers
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

-- Validate credit card number
validate n = check == 0
    where digits = toDigits n
          doubled = doubleEveryOther digits
          sum = sumDigits doubled
          check = rem sum 10

{-
The Towers of Hanoi is a classic puzzle with a solution that can be described 
recursively. 
Disks of different sizes are stacked on three pegs; the goal is to get from a 
starting configuration with all disks stacked on the first peg to an ending 
configuration with all disks stacked on the last peg

The only rules are
  - you may only move one disk at a time, and
  - a larger disk may never be stacked on top of a smaller one.
For example, as the first move all you can do is move the topmost,
smallest disk onto a different peg, since only one disk may be moved
at a time.

To move n discs (stacked in increasing size) from peg a to peg b using peg c as 
temporary storage,
  1. move n-1 discs from a to c using b as temporary storage
  2. move the top disc from a to b 
  3. move n-1 discs from c to b using a as temporary storage.
For this exercise, define a function hanoi with the following type:
  type Peg = String
  type Move = (Peg, Peg)
  hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
Given the number of discs and names for the three pegs, hanoi should return a 
list of moves to be performed to move the stack of discs from the first peg to 
the second.
-}
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c =
    hanoi (n-1) a c b ++
    hanoi 1 a b c ++
    hanoi (n-1) c b a
    
{-
To move n discs (stacked in increasing size) from peg a to peg b using peg c as 
temporary storage,
  1. move n-1 discs from a to c using b as temporary storage
  2. move the top disc from a to b 
  3. move n-1 discs from c to b using a as temporary storage.

to move 3 from a to b
  1 move 2 from a to c using b
    1 move 1  a b
    2 move 1  a c
    3 move 1  b c
  2 move 1 from a b
  3 move 2 from c b
    1 move 1 c a
    2 move 1 c b
    3 move 1 a b

 -}
