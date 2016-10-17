import Test.Hspec
import Problem

import Data.Maybe

main :: IO ()
main = hspec $ do
  describe "p1" $ it "" $
    p1 10 `shouldBe` 23
  describe "problem1" $ it "" $
    problem1 `shouldBe` 233168

