module Runner
where
import RefrigeratedRoom
import Simulation
import Data.HashMap as Map
import Control.Concurrent

type Runner = MVar (Map Id Simulation)
type Id = String

newRunner :: IO Runner 
newRunner = newMVar empty

register :: Runner -> Id -> Simulation -> IO ()
register r id s = 
    do map <- takeMVar r
       putMVar r (insert id s map)

retrieve :: Runner -> Id -> IO (Either String Simulation)
retrieve r id =
    do map <- readMVar r
       let s = Map.lookup id map
       return (case s of
                Just sim -> Right sim
                Nothing  -> Left "SIMULATION NOT FOUND")

action :: (Simulation -> Either String Simulation) -> Runner -> Id -> IO (Either String Simulation)
action a r id =
    do map <- takeMVar r
       let s = Map.lookup id map
       let (map',result) = case s of 
                            Just sim -> case a sim of 
                                            Right sim' -> (insert id sim' map, Right sim')
                                            Left msg   -> (map, Left msg)
                            Nothing  -> (map, Left "SIMULATION NOT FOUND")
       putMVar r map'
       return result
  
startSimulation :: Runner -> Id -> IO (Either String Simulation)
startSimulation = action start

stopSimulation :: Runner -> Id -> IO (Either String Simulation)
stopSimulation = action stop

reinitSimulation :: Runner -> Id -> IO (Either String Simulation)
reinitSimulation = action reinit

updateSimulation :: Runner -> Id -> IO (Either String Simulation) 
updateSimulation = action updateRoom

setPositionSimulation :: Runner -> Id -> Position -> IO (Either String Simulation)
setPositionSimulation r id pos = action (setPosition pos) r id
 
getState :: Runner -> Id -> IO (Either String String)
getState r id = 
    do s <- retrieve r id
    
       case s of 
           Right sim -> return $ Right $ "{\"status\":"++ (show (status sim)) ++
                                    ",\"position\":"++ (show (position (room sim))) ++ 
                                    ",\"temperature\":" ++ (show (temperature (room sim))) ++ "}"
           Left msg -> return $ Left msg
