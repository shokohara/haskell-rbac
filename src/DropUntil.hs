module DropUntil where

import Data.List.Tools

nums :: [Int]
nums = [0..2] ++ [0..3] ++ [0..4]
main = do
  print nums
  print $ split 0 nums [] == [[0,1,2],[0,1,2,3],[0,1,2,3,4]]
  print $ split 0 nums []

split :: Int -> [Int] -> [[Int]] -> [[Int]]
split s [] xs = xs
split s x xs = split s (f1 s x) (f2 s x : xs)
  where
    f1 s = reverse . dropUntil (== s) . reverse
    f2 s = reverse . takeUntil (== s) . reverse

