module Main
where
import Control.Monad.Trans (lift)
import Control.Monad (msum)
import Happstack.Lite
import Data.Text.Lazy (unpack)
import Simulation
import Runner 
import Text.JSON

registerSimulation :: Runner -> ServerPart Response
registerSimulation r = do
    param <- lookText "id"
    let id = read $ unpack param
    lift $ register r id newSimulation
    lift $ putStrLn $ "registering simulation with id "++id
    ok $ toResponse $ encode "OK"
    

startTheSimulation :: Runner -> ServerPart Response
startTheSimulation r = do
    param <- lookText "id"
    let id = read $ unpack param
    result <- lift $ startSimulation r id 
    let response = case result of
                    Right sim -> "OK"
                    Left msg  -> msg
    lift $ putStrLn $ "starting simulsation with id " ++ id
    ok $ toResponse $ encode response

simulationState :: Runner -> ServerPart Response
simulationState r = do
    param <- lookText "id"
    let id = read $ unpack param
    result <- lift $ getState r id
    let response = case result of
                    Right json -> json
                    Left msg   -> msg
    lift $ putStrLn $ "state with id " ++ id ++ " response:" ++ response
    ok $ toResponse response

staticFiles :: ServerPart Response
staticFiles = serveDirectory EnableBrowsing ["fridge.html", "admin.html","jquery-2.0.3.min.js"] "."

routes :: Runner -> ServerPart Response
routes r = msum [dir "state" $ simulationState r
                ,dir "register" $ registerSimulation r
                ,dir "start" $ startTheSimulation r
                ,staticFiles] 

main = do r <- newRunner
          serve Nothing $ routes r
