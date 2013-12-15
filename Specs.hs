import Test.Hspec
import RefrigeratedRoom
import Simulation
import Report

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "fromRight applied to a Left value"

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

    describe "a refrigerated room simulation\n" $ do

        it "should have a status request" $ do
            status newSimulation `shouldBe` Idle

        it "should be runnable" $ do
            status (start newSimulation) `shouldBe` Running

        it "should provide information about the room" $ do
            roomInfo newSimulation `shouldBe` (15.0, 100)

        it "should have a command to set position" $ do
            roomInfo (fromRight(setPosition (start newSimulation) 50)) `shouldBe` (15.0, 50)


        it "should have a command to update its room" $ do
            roomInfo (fromRight (updateRoom (start newSimulation))) `shouldBe`(14.0, 100)

        it "should be stoppable" $ do
            status (stop (start newSimulation)) `shouldBe` Idle

        it "should be able to reinitialize" $ do
            roomInfo (reinit (fromRight (updateRoom (start newSimulation)))) `shouldBe` (15.0, 100)
            status (reinit (start newSimulation)) `shouldBe` Idle
    
        it "should not allow to set position if not running" $ do
            setPosition newSimulation 50 `shouldBe` Left "SIMULATION NOT RUNNING"
            setPosition (stop (start newSimulation)) 50 `shouldBe` Left "SIMULATION NOT RUNNING"

        it "should not allow to update room if not running" $ do
            updateRoom newSimulation `shouldBe` Left "SIMULATION NOT RUNNING"

    describe "a report\n" $ do
        
        it "should record a room state" $ do
            report (recordState newSimulation []) `shouldBe` [(1,100,15.0)]

        it "should record successive room states" $ do
            report states' `shouldBe` [(1,100,15.0),(2,50,12.333333333333334)]
            where server = start newSimulation
                  states = recordState server []
                  server'= (fromRight (updateRoom (fromRight (setPosition server 50))))
                  states'= recordState server' states
