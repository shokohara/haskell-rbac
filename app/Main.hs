module Main where

import Control.Lens

seedList = [0..100]
effector = [1..3]
f :: [Int] -> [[Int]] -> [[Int]]
f [] n = n
f l n = f (drop 1 l) $ n ++ [take 3 l]
productTuple :: (Int, Int) -> Int
productTuple v = product $ (^..each) v
main = do
  let x = f seedList [] :: [[Int]]
  let y = map z x :: [[(Int, Int)]]
  let r = map sum $ map (map productTuple) y :: [Int]
  print r
    where
      z :: [Int] -> [(Int, Int)]
      z v = zip v effector

