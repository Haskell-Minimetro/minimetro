module ProjectSpec where

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Tests for Project module" $ do

    it "2 + 2 = 4" $ do
      2 + 2 `shouldBe` 4

    it "reverse . reverse = id" $ property $ \xs ->
      reverse (reverse xs) == (xs :: [Int])
