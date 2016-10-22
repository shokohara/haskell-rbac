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
  describe "listPrimeFactor" $ it "" $
    listPrimeFactor 13195 [] `shouldBe` [5, 7, 13, 29]
  describe "problem3" $ it "" $
    problem3 `shouldBe` 6857
  describe "palindrome" $ it "" $ do
    palindrome (show 101) `shouldBe` True
    palindrome (show 1001) `shouldBe` True
  describe "problem4" $ it "" $
    head problem4 `shouldBe` 906609
  describe "problem5" $ it "" $
    problem5 10 `shouldBe` 2520
  describe "problem6" $ it "" $
    problem6 10 `shouldBe` 3025 - 385
  describe "isPrime" $ it "" $
    map isPrime [7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47] `shouldBe` map (\n -> True) [7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
  describe "problem7" $ do
    it "premise" $ problem7 6 `shouldBe` 13
    --it "answer" $ problem7 10001 `shouldBe` 104743

