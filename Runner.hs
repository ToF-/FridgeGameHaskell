module Runner
where
import RefrigeratedRoom
import Simulation
import Data.HashMap as Map
import Control.Concurrent
import Data.Char

type Runner = MVar (Map Id Simulation)
type Id = String

newRunner :: IO Runner 
newRunner = newMVar empty

register :: Runner -> Id -> Simulation -> IO (Either String Simulation)
register r id s = 
    do map <- takeMVar r
       putMVar r (insert id s map)
       return $ Right s

action :: (Simulation -> Either String Simulation) -> Runner -> Id 
            -> IO (Either String Simulation)
action a r id =
    do map <- takeMVar r
       let (map',result) = case Map.lookup id map of 
                            Just sim -> case a sim of 
                                            Right sim' -> (insert id sim' map, Right sim')
                                            Left msg   -> (map, Left msg)
                            Nothing  -> (map, Left "SIMULATION NOT FOUND")
       putMVar r map'
       return result
  
isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

setPositionSimulation :: Runner -> Id -> String-> IO (Either String Simulation)
setPositionSimulation r id pos = 
    case isInteger pos of
        True -> action (setPosition (read pos)) r id
        False -> return $ Left $ "NOT AN INTEGER: " ++ pos
 
getState :: Runner -> Id -> IO (Either String String)
getState r id = 
    do s <- action retrieve r id
    
       case s of 
           Right sim -> return $ Right $ "{\"status\":"++ (show (show (status sim))) ++
                                    ",\"position\":"++ (show (position (room sim))) ++ 
                                    ",\"temperature\":" ++ (show (temperature (room sim))) ++ "}"
           Left msg -> return $ Left msg

updateAllSimulations :: Runner -> IO ()
updateAllSimulations r = 
    do m <- takeMVar r
       putMVar r $ Map.map updateSim m
    where updateSim sim = case updateRoom sim of 
                               Right sim' -> sim'
                               Left  _    -> sim
