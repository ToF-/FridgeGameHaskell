{-# LANGUAGE OverloadedStrings #-}
module Runner
where
import RefrigeratedRoom
import Simulation
import Data.Map as Map
import Control.Concurrent
import Data.Char
import Safe (readMay)
import Data.Aeson
import qualified Data.ByteString.Lazy as B

instance ToJSON Simulation where
    toJSON s = object ["status"   .= show (status s),
                       "position" .= position (room s),
                       "temperature" .= temperature (room s)]

type Runner = MVar (Map Id Simulation)
type Id = String

newRunner :: IO Runner 
newRunner = newMVar empty

register :: Runner -> Id -> IO (Either String Simulation)
register r id = 
    do map <- takeMVar r
       let s = newSimulation
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
  
setPositionSimulation :: String -> Runner -> Id -> IO (Either String Simulation)
setPositionSimulation pos r id = 
    case readMay pos of
        Just n  -> action (setPosition n) r id
        Nothing -> return $ Left $ "NOT AN INTEGER: " ++ pos
 
getState :: Runner -> Id -> IO (Either String B.ByteString)
getState r id = 
    do s <- action retrieve r id
    
       case s of 
           Right sim -> return $ Right $ encode sim
           Left msg -> return $ Left msg

updateAllSimulations :: Runner -> IO ()
updateAllSimulations r = 
    do m <- takeMVar r
       putMVar r $ Map.map updateSim m
    where updateSim sim = case updateRoom sim of 
                               Right sim' -> sim'
                               Left  _    -> sim
