module Problem where

import Data.List (sort, sortBy, find)
import Data.Maybe (fromJust, maybeToList)
import Data.Char (digitToInt)

problem1 = p1 1000

p1 n = sum [ x | x <- [1..n-1], x `mod` 3 == 0 || x `mod` 5 == 0]

problem2 :: Integer
problem2 = sum . filter even . takeWhile (<=4000000) $ fibs

fib n = fibs!!n
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

problem3 :: Integer
problem3 = maximum $ listPrimeFactor 600851475143 []

listPrimeFactor :: Integer -> [Integer] -> [Integer]
listPrimeFactor 1 ns = ns
listPrimeFactor n ns = do
  let x = find (\x -> n `mod` x == 0) [2..] :: Maybe Integer
  listPrimeFactor (n `div` fromJust x) (ns ++ maybeToList x)

problem4 :: [Integer]
problem4 = largestPalindromeProduct [100..999] [100..999]

largestPalindromeProduct :: [Integer] -> [Integer] -> [Integer]
largestPalindromeProduct xs ys =
  take 1 . sortBy (flip compare) . filter (palindrome . show) $ [ x * y | x <- sortBy (flip compare) xs, y <- sortBy (flip compare) ys]

palindrome :: String -> Bool
palindrome x = x == reverse x

problem5 :: Integer -> Integer
problem5 x = head [s | s <- [1..], all (\v -> s `mod` v == 0) (reverse [1..x])]

problem6 :: Integer -> Integer
problem6 x = (square . sum $ [1..x]) - sum (map square [1..x])

square x = x * x

problem7 :: Int -> Integer
problem7 x = last . take x . filter isPrime $ [2..]

isPrime :: Integer -> Bool
isPrime x = all (\n -> 0 /= x `mod` n) [2..x-1]

problem8 :: String -> Int -> Int
problem8 xs i = maximum . map product . listSeries [] i . stringToListInt $ xs

listSeries :: [[Int]] -> Int -> [Int] -> [[Int]]
listSeries r i xs = if length xs < i then r else listSeries (r ++ [take i xs]) i (drop 1 xs)

stringToListInt :: String -> [Int]
stringToListInt x = map digitToInt $ x

problem10 x = sum . filter isPrime $ [2..x-1]

