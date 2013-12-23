module Main
where
import Control.Monad.Trans (lift)
import Control.Monad (msum)
import Happstack.Lite
import Data.Text.Lazy (unpack)
import Simulation
import Runner

registerSimulation :: Runner -> ServerPart Response
registerSimulation r = do
    param <- lookText "id"
    let id = read $ unpack param
    lift $ register r id newSimulation
    lift $ startSimulation r id 
    lift $ putStrLn $ "registering simuation with id "++id
    ok $ toResponse "OK"
    
 
getSimulationState :: Runner -> ServerPart Response
getSimulationState r = do
    param <- lookText "id"
    let id = read $ unpack param
    result <- lift $ getState r id
    let response = case result of
                    Right json -> json
                    Left msg   -> msg
    ok $ toResponse response

staticFiles :: ServerPart Response
staticFiles = serveDirectory EnableBrowsing ["fridge.html"] "."

routes :: Runner -> ServerPart Response
routes r = msum [dir "getSimulationState" $ getSimulationState r
                ,dir "register" $ registerSimulation r
                ,staticFiles] 

main = do r <- newRunner
          serve Nothing $ routes r
