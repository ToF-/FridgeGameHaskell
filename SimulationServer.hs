module Main
where
import System.Environment (getArgs)
import Control.Monad.Trans (lift)
import Control.Monad (msum)
import Happstack.Lite (ServerPart, Response, ok, dir, lookText, 
                       toResponse, serveDirectory, Browsing(EnableBrowsing), serve)
import Data.Text.Lazy (unpack)
import Simulation (newSimulation, start, stop)
import Runner (Runner, getState, register, Id, action, 
               updateAllSimulations, newRunner, setPositionSimulation)
import Text.JSON (encode)
import Control.Concurrent.Timer (newTimer, repeatedStart, stopTimer)
import Control.Concurrent.Suspend.Lifted (Delay, sDelay)

logLn :: String -> ServerPart ()
logLn = lift . putStrLn

respond :: String -> ServerPart Response
respond = ok . toResponse . encode

getIdParam :: ServerPart Id
getIdParam = do
    param <- lookText "id"
    return $ read $ unpack param

registerSimulation :: Runner -> ServerPart Response
registerSimulation r = do
    id <- getIdParam
    lift $ register r id newSimulation
    logLn $ "registering simulation with id "++id
    respond "OK"
    

startTheSimulation :: Runner -> ServerPart Response
startTheSimulation r = do
    id <- getIdParam
    result <- lift $ action start r id 
    let response = case result of
                    Right sim -> "OK"
                    Left msg  -> msg
    logLn $ "starting simulsation with id " ++ id
    respond response

stopTheSimulation :: Runner -> ServerPart Response
stopTheSimulation r = do
    id <- getIdParam
    result <- lift $ action stop r id 
    let response = case result of
                    Right sim -> "OK"
                    Left msg  -> msg
    logLn $ "stopping simulsation with id " ++ id
    respond response

simulationState :: Runner -> ServerPart Response
simulationState r = do
    id <- getIdParam
    result <- lift $ getState r id
    let response = case result of
                    Right json -> json
                    Left msg   -> msg
    logLn $ "state with id " ++ id ++ " response:" ++ response
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
    logLn $ "set simulation with id " ++ id ++ " to position " ++ (show pos) ++ " :" ++ response
    respond response
    

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
