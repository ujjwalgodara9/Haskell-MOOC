-- Exercise set 4b: folds

module Set4b where

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: countNothings with a fold. The function countNothings from
-- the course material can be implemented using foldr. Your task is to
-- define countHelper so that the following definition of countNothings
-- works.
--
-- Hint: You can start by trying to add a type signature for countHelper.
--
-- Challenge: look up the maybe function and use it in countHelper.
--
-- Examples:
--   countNothings []  ==>  0
--   countNothings [Just 1, Nothing, Just 3, Nothing]  ==>  2

countHelper :: Maybe a -> Int -> Int
countHelper Nothing acc = acc + 1
countHelper (Just _) acc = acc

countNothings :: [Maybe a] -> Int
countNothings = foldr countHelper 0

------------------------------------------------------------------------------
-- Ex 2: myMaximum with a fold. Just like in the previous exercise,
-- define maxHelper so that the given definition of myMaximum works.
--
-- Examples:
--   myMaximum []  ==>  0
--   myMaximum [1,3,2]  ==>  3

maxHelper :: Int -> Int -> Int
maxHelper x acc = max x acc

myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum (x:xs) = foldr maxHelper x xs


------------------------------------------------------------------------------
-- Ex 3: compute the sum and length of a list with a fold. Define
-- slHelper and slStart so that the given definition of sumAndLength
-- works. This could be used to compute the average of a list.
--
-- Start by giving slStart and slHelper types.
--
-- Examples:
--   sumAndLength []             ==>  (0.0,0)
--   sumAndLength [1.0,2.0,4.0]  ==>  (7.0,3)


slStart :: (Double, Int)
slStart = (0.0, 0)

slHelper :: Double -> (Double, Int) -> (Double, Int)
slHelper x (sum, length) = (sum + x, length + 1)

sumAndLength :: [Double] -> (Double, Int)
sumAndLength xs = foldr slHelper slStart xs


------------------------------------------------------------------------------
-- Ex 4: implement concat with a fold. Define concatHelper and
-- concatStart so that the given definition of myConcat joins inner
-- lists of a list.
--
-- Examples:
--   myConcat [[]]                ==> []
--   myConcat [[1,2,3],[4,5],[6]] ==> [1,2,3,4,5,6]

concatStart :: [a]
concatStart = []

concatHelper :: [a] -> [a] -> [a]
concatHelper x acc = x ++ acc

myConcat :: [[a]] -> [a]
myConcat xs = foldr concatHelper concatStart xs


------------------------------------------------------------------------------
-- Ex 5: get all occurrences of the largest number in a list with a
-- fold. Implement largestHelper so that the given definition of largest works.
--
-- Examples:
--   largest [] ==> []
--   largest [1,3,2] ==> [3]
--   largest [1,3,2,3] ==> [3,3]

largestHelper :: Int -> [Int] -> [Int]
largestHelper x [] = [x]
largestHelper x acc
  | x > head acc = [x]
  | x == head acc = x : acc
  | otherwise = acc

largest :: [Int] -> [Int]
largest xs = foldr largestHelper [] xs



------------------------------------------------------------------------------
-- Ex 6: get the first element of a list with a fold. Define
-- headHelper so that the given definition of myHead works.
--
-- Start by giving headHelper a type.
--
-- Examples:
--   myHead []  ==>  Nothing
--   myHead [1,2,3]  ==>  Just 1

headHelper :: a -> Maybe a -> Maybe a
headHelper x _ = Just x

myHead :: [a] -> Maybe a
myHead xs = foldr headHelper Nothing xs

------------------------------------------------------------------------------
-- Ex 7: get the last element of a list with a fold. Define lasthelper
-- so that the given definition of myLast works.
--
-- Start by giving lastHelper a type.
--
-- Examples:
--   myLast [] ==> Nothing
--   myLast [1,2,3] ==> Just 3

lastHelper :: a -> Maybe a -> Maybe a
lastHelper x Nothing = Just x
lastHelper _ acc = acc

myLast :: [a] -> Maybe a
myLast xs = foldr lastHelper Nothing (reverse xs)

