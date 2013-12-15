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


startSimulation :: Runner -> Id -> IO ()
startSimulation r id =
    do map <- takeMVar r
       let s = Map.lookup id map
       let map' = case s of 
                    Just sim -> insert id (start sim) map
                    Nothing -> map
       putMVar r map'


stopSimulation :: Runner -> Id -> IO ()
stopSimulation r id =
    do map <- takeMVar r
       let s = Map.lookup id map
       let map' = case s of 
                    Just sim -> insert id (stop sim) map
                    Nothing -> map
       putMVar r map'

reinitSimulation :: Runner -> Id -> IO ()
reinitSimulation r id = 
    do map <- takeMVar r
       let s = Map.lookup id map
       let map' = case s of 
                    Just sim -> insert id (reinit sim) map
                    Nothing -> map
       putMVar r map'

updateSimulation :: Runner -> Id -> IO ()
updateSimulation r id = 
    do map <- takeMVar r
       let s = Map.lookup id map
       let map' = case s of
                    Just sim -> insert id (case updateRoom sim of 
                                              Right sim' -> sim'
                                              Left _ -> sim)      map
                    Nothing -> map
       putMVar r map'

setPositionSimulation :: Runner -> Id -> Position -> IO ()
setPositionSimulation r id pos = 
    do map <- takeMVar r
       let s = Map.lookup id map
       let map' = case s of
                    Just sim -> insert id (case setPosition sim pos of 
                                              Right sim' -> sim'
                                              Left _ -> sim)      map
                    Nothing -> map
       putMVar r map'
