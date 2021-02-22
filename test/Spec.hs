import Test.Hspec
import Cue

main :: IO ()
main = hspec $ do

  describe "instance Show Time" $ do
    it "Time 23 45 67" $ do
      show (Time 23 45 67) `shouldBe` "234567"

  describe "instance Num Time" $ do

    it "identity" $ do
      (Time 5 39 19) `sub` (Time 0 0 0) `shouldBe` Time 5 39 19
      
    it "underflow frame" $ do
      (Time 5 39 19) `sub` (Time 0 0 20) `shouldBe` Time 5 38 74

    it "underflow sec" $ do
      (Time 5 39 19) `sub` (Time 0 40 0) `shouldBe` Time 4 59 19

