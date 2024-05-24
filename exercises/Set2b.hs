module Set2b where

import Mooc.Todo

-- Some imports you'll need. Don't add other imports :)
import Data.List

------------------------------------------------------------------------------
-- Ex 1: compute binomial coefficients using recursion. Binomial
-- coefficients are defined by the following equations:
--
--   B(n,k) = B(n-1,k) + B(n-1,k-1)
--   B(n,0) = 1
--   B(0,k) = 0, when k>0
--
-- Hint! pattern matching is your friend.

binomial :: Integer -> Integer -> Integer
binomial n k
  | k == 0 || n == k = 1  -- Base cases: B(n, 0) and B(n, n)
  | k < 0 || n < 0    = error "n and k must be non-negative integers" -- Handle errors
  | otherwise         = binomial (n-1) k + binomial (n-1) (k-1) -- Recursive case


------------------------------------------------------------------------------
-- Ex 2: implement the odd factorial function. Odd factorial is like
-- factorial, but it only multiplies odd numbers.
--
-- Examples:
--   oddFactorial 7 ==> 7*5*3*1 ==> 105
--   oddFactorial 6 ==> 5*3*1 ==> 15

oddFactorial :: Integer -> Integer
oddFactorial n
  | n == 1 = 1  -- Base case
  | even n = oddFactorial (n - 1)  -- Recursive case, filtering even numbers
  | otherwise = n * oddFactorial (n - 2) -- Recursive case, filtering even numbers


------------------------------------------------------------------------------
-- Ex 3: implement the Euclidean Algorithm for finding the greatest
-- common divisor:
--
-- Given two numbers, a and b,
-- * if one is zero, return the other number
-- * if not, subtract the smaller number from the larger one
-- * replace the larger number with this new number
-- * repeat
--
-- For example,
--   myGcd 9 12 ==> 3
-- In this case, the algorithm proceeds like this
--
--   a      b
--
--   9      12
--   9      (12-9)
--   9      3
--   (9-3)  3
--   6      3
--   (6-3)  3
--   3      3
--   (3-3)  3
--   0      3
--
-- Background reading:
-- * https://en.wikipedia.org/wiki/Euclidean_algorithm

myGcd :: Integer -> Integer -> Integer
myGcd a b
  | b == 0    = a  -- Base case: if b is zero, return a
  | otherwise = myGcd b (a `mod` b)  -- Recursive case: replace larger with remainder


------------------------------------------------------------------------------
-- Ex 4: Implement the function leftpad which adds space characters
-- to the start of the string until it is long enough.
--
-- Examples:
--   leftpad "foo" 5 ==> "  foo"
--   leftpad "13" 3 ==> " 13"
--   leftpad "xxxxx" 3 ==> "xxxxx"
--
-- Tips:
-- * you can combine strings with the ++ operator.
-- * you can compute the length of a string with the length function

leftpad :: String -> Int -> String
leftpad str desiredLength
  | length str >= desiredLength = str  -- No padding needed
  | otherwise                   =leftpad' str (desiredLength - length str) -- Recursively prepend spaces

leftpad' :: String -> Int -> String
leftpad' str 0 = str
leftpad' str desiredLength = leftpad' (" " ++ str) (desiredLength-1)

------------------------------------------------------------------------------
-- Ex 5: let's make a countdown for a rocket! Given a number, you
-- should produce a string that says "Ready!", counts down from the
-- number, and then says "Liftoff!".
--
-- For example,
--   countdown 4 ==> "Ready! 4... 3... 2... 1... Liftoff!"
--
-- Hints:
-- * you can combine strings with the ++ operator
-- * you can use the show function to convert a number into a string
-- * you'll probably need a recursive helper function

countdown :: Integer -> String
countdown n = "Ready! " ++ countdown' n

countdown' :: Integer -> String
countdown' 0 = "Liftoff!"
countdown' n = show n ++ "... " ++ countdown' (n-1)


------------------------------------------------------------------------------
-- Ex 6: implement the function smallestDivisor that returns the
-- smallest number (greater than 1) that divides the given number evenly.
--
-- That is, when
--   smallestDivisor n ==> k
-- we have
--   n = t*k
-- for some t.
--
-- Ps. your function doesn't need to work for inputs 0 and 1, but
-- remember this in the next exercise!
--
-- Hint: remember the mod function!

smallestDivisor :: Integer -> Integer
smallestDivisor n
  | n <= 1 = error "n must be greater than 1"  -- Handle errors
  | even n = 2  -- Check for divisibility by 2 first (efficiency)
  | otherwise = findDivisor n 3  -- Recursively check odd numbers

findDivisor :: Integer -> Integer -> Integer
findDivisor n divisor
  | divisor * divisor > n = n  -- No divisors found, return n itself
  | n `mod` divisor == 0 = divisor  -- Found a divisor
  | otherwise            = findDivisor n (divisor + 2)  -- Check next odd number

------------------------------------------------------------------------------
-- Ex 7: implement a function isPrime that checks if the given number
-- is a prime number. Use the function smallestDivisor.
--
-- Ps. 0 and 1 are not prime numbers

isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False  -- 0 and 1 are not prime
  | n == smallestDivisor n = True  -- Handle edge case: n is its own smallest divisor (prime)
  | otherwise             = smallestDivisor n > n  -- If a smaller divisor exists, n is not prime

------------------------------------------------------------------------------
-- Ex 8: implement a function biggestPrimeAtMost that returns the
-- biggest prime number that is less than or equal to the given
-- number. Use the function isPrime you just defined.
--
-- You don't need to care about arguments less than 2. Any behaviour
-- for them is fine.
--
-- Examples:
--   biggestPrimeAtMost 3 ==> 3
--   biggestPrimeAtMost 10 ==> 7

biggestPrimeAtMost :: Integer -> Integer
biggestPrimeAtMost x
  | x < 2 = error "n must be greater than or equal to 2"  -- Handle invalid inputs
  | otherwise = biggestPrimeHelper x

biggestPrimeHelper :: Integer -> Integer
biggestPrimeHelper x
  | isPrime x = x   -- Found a prime, stop searching
  | otherwise = biggestPrimeHelper (pred x)  -- Use pred x for efficiency (previous number)
