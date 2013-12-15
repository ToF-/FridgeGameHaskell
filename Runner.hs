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

action :: (Simulation -> Simulation) -> Runner -> Id -> IO ()
action a r id =
    do map <- takeMVar r
       let s = Map.lookup id map
       let map' = case s of 
                    Just sim -> insert id (a sim) map
                    Nothing  -> map
       putMVar r map'
  
startSimulation :: Runner -> Id -> IO ()
startSimulation = action start

stopSimulation :: Runner -> Id -> IO ()
stopSimulation = action stop

reinitSimulation :: Runner -> Id -> IO ()
reinitSimulation = action reinit

updateSimulation :: Runner -> Id -> IO ()
updateSimulation = action (\sim -> case updateRoom sim of 
                                      Right sim' -> sim'
                                      Left _     -> sim) 

setPositionSimulation :: Runner -> Id -> Position -> IO ()
setPositionSimulation r id pos = action (\sim -> case setPosition sim pos of 
                                                    Right sim' -> sim'
                                                    Left _ -> sim) r id
