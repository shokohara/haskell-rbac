module Problem where

import Data.List (sort, sortBy, find)
import Data.Maybe (fromJust, maybeToList)
import Data.Char (digitToInt)
import Data.Matrix (fromLists, toLists, transpose)
import qualified Data.Map as Map

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

problem11InputParse :: String -> [[Int]]
problem11InputParse = map (map stringToInt . words) . lines

problem11 :: [[Int]] -> [[Int]]
problem11 x = (concatMap (listSeries [] 4) $ toLists . transpose . fromLists $ x) ++ (concatMap (listSeries [] 4) $ x)

stringToInt :: String -> Int
stringToInt x = read x :: Int

problem13 :: [Integer] -> Int
problem13 x = stringToInt . take 10 . show . sum $ x

problem14 :: Int -> Int
problem14 x = maximum . map length . map (collatzSequence []) $ [1..x-1]

collatzSequence :: [Int] -> Int -> [Int]
collatzSequence r 1 = r
collatzSequence [] n = collatzSequence [n] n
collatzSequence r n = collatzSequence (r ++ [if even n then n `div` 2 else 3 * n + 1]) (if even n then n `div` 2 else 3 * n + 1)

-- 0countGrid x = p

--problem16 x = sum . stringToListInt . show $ 2 ^ x

--problem17 x = length . filter (/= '-') . filter (/= ' ') . concatMap numberToWords $ [1..x]

--numberToWords x =
--  Map.lookup x . Map.fromList [(1, "one")
--                              ,(2, "two")
--                              ,(3, "three")
--                              ,(4, "four")
--                              ,(5, "five")
--                              ,(6, "six")
--                              ,(7, "seven")
--                              ,(8, "eight")
--                              ,(9, "nine")
--                              ,(10, "ten")
--                              ,(11, "eleven")
--                              ,(12, "twelve")
--                              ,(13, "thirteen")
--                              ,(14, "fourteen")
--                              ,(15, "fifteen")
--                              ,(16, "sixteen")
--                              ,(17, "seventeen")
--                              ,(18, "eighteen")
--                              ,(19, "nineteen")
--                              ,(20, "twenty")
--                              ,(30, "thirty")
--                              ,(40, "forty")
--                              ,(50, "fifty")
--                              ,(60, "sixty")
--                              ,(70, "seventy")
--                              ,(80, "eighty")
--                              ,(90, "ninety")
--                              ,(100, "one hundred")
--                              ,(1000, "one thousand")]
--
