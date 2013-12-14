import Test.Hspec
import RefrigeratedRoom

apply :: Int -> (a -> a) -> a -> a
apply n f i = (iterate f i) !! n

main :: IO ()
main = hspec $ do
    describe "A refrigerated room\n" $ do
        it "should have an initial temperature of 15.0" $ do
            temperature newRoom `shouldBe` 15.0

        it "should have an inital position set to 100" $ do
            position newRoom `shouldBe`100

        it "should have its temperature evolve after update" $ do
            temperature (update newRoom) `shouldBe` 14.0
            temperature (apply 2 update newRoom) `shouldBe` 13.0

        it "should allow its position to be modified" $ do
            position (newRoom { position = 50 }) `shouldBe` 50

        it "should have its temperature decreasing differently after 5 updates" $ do
            temperature (apply 6 update newRoom) `shouldBe` 9.333333333333334   

        it "should have its temperature decreasing differently according to position" $ do
            temperature (update (newRoom { position = 50 })) `shouldBe` 12.333333333333334
