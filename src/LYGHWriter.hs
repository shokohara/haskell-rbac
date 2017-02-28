module LYGHWriter where

import Control.Monad.Writer
import Data.Tree
import Text.Read
import Data.Maybe

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  return (a*b)

--gcd' a b
--  | b == 0 = a
--  | otherwise = gcd' b (a `mod` b)

--gcd' :: Int -> Int -> Writer [String] Int
--gcd' a b
--  | b == 0 = do
--    tell ["Finished with " ++ show a]
--    return a
--  | otherwise = do
--    tell ([show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
--    gcd' b (a `mod` b)

--gcdReverse :: Int -> Int -> Writer [String] Int
--gcdReverse a b
--  | b == 0 = do
--    tell ["Finished with " ++ show a]
--    return a
--  | otherwise = do
--    result <- gcdReverse b (a `mod` b)
--    tell ([show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
--    return result

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

gcd' :: Int -> Int -> Writer (DiffList String) Int
gcd' a b
  | b == 0 = do
    tell (toDiffList ["Finished with " ++ show a])
    return a
  | otherwise = do
    result <- gcd' b (a `mod` b)
    tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
    return result

gcdReverse :: Int -> Int -> Writer (DiffList String) Int
gcdReverse a b
  | b == 0 = do
    tell (toDiffList ["Finished with " ++ show a])
    return a
  | otherwise = do
    result <- gcdReverse b (a `mod` b)
    tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
    return result

--finalCountDown :: Int -> Writer (DiffList String) ()
--finalCountDown 0 = do
--  tell (toDiffList ["0"])
--finalCountDown x = do
--  finalCountDown (x-1)
--  tell (toDiffList [show x])

finalCountDown :: Int -> Writer [String] ()
finalCountDown 0 = do
  tell ["0"]
finalCountDown x = do
  finalCountDown (x-1)
  tell [show x]

input = "50\n10\n30\n5\n90\n20\n40\n2\n25\n10\n8\n0"

loads :: String -> [Int]
loads = catMaybes . fmap readMaybe . lines

structuredLoad :: [Int] -> [[Int]]
structuredLoad xs = [take 2 xs] ++ [take 1 . drop 2 $ xs] ++ [take 2 . drop 3 $ xs] ++ [take 1 . drop 5 $ xs] ++ [take 2 . drop 6 $ xs] ++ [take 1 . drop 8 $ xs] ++ [take 2 . drop 9 $ xs] ++ [take 1 . drop 11 $ xs]

x = unfoldTree (\x -> (x, [])) 1

