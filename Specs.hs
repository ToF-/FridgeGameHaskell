import Test.Hspec
import RefrigeratedRoom

main :: IO ()
main = hspec $ do
    describe "A refrigerated room\n" $ do
        it "should have an initial temperature of 15.0" $ do
            temperature newRoom `shouldBe` 15.0

        it "should have an inital position set to 100" $ do
            position newRoom `shouldBe`100

        it "should have its temperature evolve after update" $ do
            temperature (update newRoom) `shouldBe` 14.0
            temperature (update (update newRoom)) `shouldBe` 13.0

        it "should allow its position to be modified" $ do
            position (newRoom { position = 50 }) `shouldBe` 50

           
