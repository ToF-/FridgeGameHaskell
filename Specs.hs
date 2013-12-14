import Test.Hspec
import RefrigeratedRoom

main :: IO ()
main = hspec $ do
    describe "A refrigerated room\n" $ do
        it "should have a initial temperature of 15.0" $ do
            temperature newRoom `shouldBe` 15.0
