import Test.Hspec
import Problem

import Data.Maybe

main :: IO ()
main = hspec $ do
  describe "p1" $ it "" $
    p1 10 `shouldBe` 23
  describe "problem1" $ it "" $
    problem1 `shouldBe` 233168
  describe "fib" $ it "" $
    map fib [0..9] `shouldBe` [1, 2, 3, 5, 8, 13, 21, 34, 55, 89]
  describe "problem2" $ it "" $
    problem2 `shouldBe` 4613732

