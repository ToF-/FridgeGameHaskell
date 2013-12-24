import Test.Hspec
import RefrigeratedRoom
import Simulation
import Report
import Runner
import Data.Maybe

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

        it "should have an initial position set to 100" $ do
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
            status (fromRight $ start newSimulation) `shouldBe` Running

        it "should provide information about the room" $ do
            roomInfo newSimulation `shouldBe` (15.0, 100)

        it "should have a command to set position" $ do
            roomInfo (fromRight(setPosition 50 (fromRight $ start newSimulation))) `shouldBe` (15.0, 50)

        it "should have a command to update its room" $ do
            roomInfo (fromRight (updateRoom (fromRight $ start newSimulation))) `shouldBe`(14.0, 100)

        it "should be stoppable" $ do
            status (fromRight $ stop (fromRight $ start newSimulation)) `shouldBe` Idle

        it "should be able to reinitialize" $ do
            roomInfo (fromRight $ reinit (fromRight (updateRoom (fromRight $ start newSimulation)))) `shouldBe` (15.0, 100)
            status (fromRight $ reinit (fromRight $ start newSimulation)) `shouldBe` Idle
    
        it "should not allow to set position if not running" $ do
            setPosition 50 newSimulation `shouldBe` Left "SIMULATION NOT RUNNING"
            setPosition 50 (fromRight $ stop (fromRight $ start newSimulation)) `shouldBe` Left "SIMULATION NOT RUNNING"

        it "should not allow to update room if not running" $ do
            updateRoom newSimulation `shouldBe` Left "SIMULATION NOT RUNNING"

    describe "a report\n" $ do

        let simulation = fromRight $ updateRoom $ fromRight $ start newSimulation
            simulation'= fromRight $ updateRoom $ fromRight $ setPosition 50 simulation

        it "should be empty for a new simulation" $ do 
            report newSimulation `shouldBe` []
        
        it "should record a room state" $ do
            report simulation `shouldBe` [(1,100,15.0)]

        it "should record successive room states" $ do
            report simulation' `shouldBe` [(1,100,15.0),(2,50,14)]

        it "should pretty print the states" $ do
            pretty (report simulation') `shouldBe` "1\t100\t15.0\n2\t50\t14.0\n"    

        it "should print the current state" $ do
             displaySimulationState simulation `shouldBe` "STATUS:Running POSITION:100 TEMPERATURE:14.0"
            
        it "should round temperature display" $ do
            let simulation'' = fromRight $ updateRoom $ simulation'
            displaySimulationState simulation'' `shouldBe` "STATUS:Running POSITION:50 TEMPERATURE:8.7"

    describe "a simulation runner\n" $ do

        let runnerFor id = do r <- newRunner
                              register r id newSimulation
                              return r

        let find r id = do s <- action retrieve r id 
                           return (fromRight s)

        it "should memorize and retrieve a simulation with an id" $ do
            r <- runnerFor "CHRIS"
            s <- find r "CHRIS"
            status s `shouldBe` Idle
            
        it "should start a simulation" $ do
            r <- runnerFor "CHRIS"
            action start r "CHRIS"
            s <- find r "CHRIS"
            status s `shouldBe` Running

        it "should stop a simulation" $ do
            r <- runnerFor "CHRIS"
            action start r "CHRIS"
            action stop r "CHRIS"
            s <- find r "CHRIS"
            status s `shouldBe` Idle

        it "should update a simulation" $ do
            r <- runnerFor "CHRIS"
            action start r "CHRIS"
            action updateRoom r "CHRIS"
            s <- find r "CHRIS"
            temperature (room s) `shouldBe` 14.0

        it "should set the position of a simulation" $ do
            r <- runnerFor "CHRIS"
            action start r "CHRIS"
            setPositionSimulation r "CHRIS" "50"
            s <- find r "CHRIS"
            position (room s) `shouldBe` 50
             
        it "should reinit a simulation" $ do
            r <- runnerFor "CHRIS"
            action start r "CHRIS"
            action updateRoom r "CHRIS"
            action reinit r "CHRIS"
            s <- find r "CHRIS"
            temperature (room s) `shouldBe` 15.0 

        it "should signal when id not found" $ do
            r <- runnerFor "CHRIS"
            s <- action retrieve r "TOF"
            s `shouldBe` Left "SIMULATION NOT FOUND"

        it "should signal when attempting to update a simulation not started" $ do
            r <- runnerFor "CHRIS"
            s <- action updateRoom r "CHRIS"
            s `shouldBe` Left "SIMULATION NOT RUNNING"

        it "should signal when attempting to set position to a negative number" $ do
            r <- runnerFor "CHRIS"
            action start r "CHRIS"
            s <- setPositionSimulation r "CHRIS" "-1"
            s `shouldBe` Left "POSITION SHOULD BE WITHIN RANGE [0..200]"

        it "should signal when attempting to set position to a number greater than 200" $ do
            r <- runnerFor "CHRIS"
            action start r "CHRIS"
            s <- setPositionSimulation r "CHRIS" "201"
            s `shouldBe` Left "POSITION SHOULD BE WITHIN RANGE [0..200]"

        it "should signal when attempting to set position to a non integer value" $ do
            r <- runnerFor "CHRIS"
            action start r "CHRIS"
            s <- setPositionSimulation r "CHRIS" "ERR"
            s `shouldBe` Left "NOT AN INTEGER: ERR"

        it "should communicate simulation information" $ do
            r <- runnerFor "CHRIS"
            json <- getState r "CHRIS"
            json `shouldBe` Right "{\"status\":\"Idle\",\"position\":100,\"temperature\":15.0}"

        it "should communicate an error if simulation unknown" $ do
            r <- runnerFor "CHRIS"
            json <- getState r "TOF"
            json `shouldBe` Left "SIMULATION NOT FOUND"

        it "should update all simulations in a row" $ do
            r <- newRunner
            register r "CHRIS" newSimulation
            register r "TOF" newSimulation
            action start r "CHRIS"
            action start r "TOF"
            updateAllSimulations r
            s <- find r "CHRIS"
            temperature (room s) `shouldBe` 14.0
            s <- find r "TOF"
            temperature (room s) `shouldBe` 14.0
            

            


