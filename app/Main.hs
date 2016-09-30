module Main where

import Control.Lens

main = do
  let seedList = [0..100]
  let effector = [1..3]
  print $ map (sum . zipWith (curry productTuple) effector) $ f seedList []
    where
      f :: [Int] -> [[Int]] -> [[Int]]
      f [] n = n
      f l n = f (drop 1 l) (n ++ [take 3 l])
      productTuple :: (Int, Int) -> Int
      productTuple v = product $ (^..each) v

