module Problem where

import Data.List (sort, sortBy, find)
import Data.Maybe (fromJust, maybeToList)

problem1 = p1 1000

p1 n = sum [ x | x <- [1..n-1], x `mod` 3 == 0 || x `mod` 5 == 0]

problem2 :: Integer
problem2 = sum . filter even . takeWhile (<=4000000) $ fibs

fib n = fibs!!n
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

