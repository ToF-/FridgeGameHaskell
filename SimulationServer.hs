module Main
where
import System.Environment
import Control.Monad.Trans (lift)
import Control.Monad (msum)
import Happstack.Lite
import Data.Text.Lazy (unpack)
import Simulation
import Runner 
import Text.JSON
import Control.Concurrent.Timer (newTimer, repeatedStart, stopTimer)
import Control.Concurrent.Suspend.Lifted (Delay, sDelay)

getIdParam :: ServerPart Id
getIdParam = do
    param <- lookText "id"
    return $ read $ unpack param

registerSimulation :: Runner -> ServerPart Response
registerSimulation r = do
    id <- getIdParam
    lift $ register r id newSimulation
    lift $ putStrLn $ "registering simulation with id "++id
    ok $ toResponse $ encode "OK"
    

startTheSimulation :: Runner -> ServerPart Response
startTheSimulation r = do
    id <- getIdParam
    result <- lift $ startSimulation r id 
    let response = case result of
                    Right sim -> "OK"
                    Left msg  -> msg
    lift $ putStrLn $ "starting simulsation with id " ++ id
    ok $ toResponse $ encode response

stopTheSimulation :: Runner -> ServerPart Response
stopTheSimulation r = do
    id <- getIdParam
    result <- lift $ stopSimulation r id 
    let response = case result of
                    Right sim -> "OK"
                    Left msg  -> msg
    lift $ putStrLn $ "stopping simulsation with id " ++ id
    ok $ toResponse $ encode response

simulationState :: Runner -> ServerPart Response
simulationState r = do
    id <- getIdParam
    result <- lift $ getState r id
    let response = case result of
                    Right json -> json
                    Left msg   -> msg
    lift $ putStrLn $ "state with id " ++ id ++ " response:" ++ response
    ok $ toResponse response

setThePosition :: Runner -> ServerPart Response
setThePosition r = do
    id <- getIdParam
    param <- lookText "pos"
    let pos = read $ unpack param
    result <- lift $ setPositionSimulation r id pos
    let response = case result of
                    Right _ -> "OK"
                    Left msg -> msg
    lift $ putStrLn $ "set simulation with id " ++ id ++ " to position " ++ (show pos) ++ " :" ++ response
    ok $ toResponse $ encode response
    

staticFiles :: ServerPart Response
staticFiles = serveDirectory EnableBrowsing ["fridge.html", "admin.html","jquery-2.0.3.min.js"] "."

routes :: Runner -> ServerPart Response
routes r = msum [dir "state" $ simulationState r
                ,dir "register" $ registerSimulation r
                ,dir "start" $ startTheSimulation r
                ,dir "stop"  $ stopTheSimulation r
                ,dir "position" $ setThePosition r
                ,staticFiles] 

updateSimulations :: Runner -> IO ()
updateSimulations r = do updateAllSimulations r
                         putStrLn "updating all simulations.."

main = do args <- getArgs
          let d = sDelay (read (args !! 0))
          r <- newRunner
          t <- newTimer
          repeatedStart t (updateSimulations r) d
          serve Nothing $ routes r
