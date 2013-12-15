module Main
where
import Control.Concurrent
import Simulation
import Report

printInstructions :: IO ()
printInstructions = putStrLn $ unlines
     ["REFRIGERATED ROOM SIMULATION"
     ,"START     : start the simulator"
     ,"POS   <n> : set the position [0..100] for the command"
     ,"<empty>   : let the simulator unchanged"
     ,"STOP      : stop the simulator"
     ,"REINIT    : put the simulator back to initial state"
     ,"HELP      : print these instructions"
     ,"QUIT      : exit the simulator"]

printSimulation :: MVar Simulation -> IO ()
printSimulation v = do s <- readMVar v
                       putStrLn $ showSimulation s

updateSimulation :: MVar Simulation -> IO ()
updateSimulation v = 
    do s <- takeMVar v
       let s' = case updateRoom s of
            Right u -> u
            Left  _ -> s
       putMVar v s'

startSimulation :: MVar Simulation -> IO ()
startSimulation v = 
    do putStrLn "STARTING THE SIMULATION"
       s <- takeMVar v
       let s' = start s
       putMVar v s'

stopSimulation :: MVar Simulation -> IO ()
stopSimulation v = 
    do putStrLn "STOPPING THE SIMULATION"
       s <- takeMVar v
       let s' = stop s
       putMVar v s'

reinitSimulation :: MVar Simulation -> IO ()
reinitSimulation v =
    do s <- takeMVar v
       let s' = newSimulation 
       putMVar v s'

isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

setPositionSimulation :: String ->MVar Simulation -> IO ()
setPositionSimulation p v = 
    case isInteger p of
        False -> do putStrLn ("NOT AN INTEGER:" ++ p)
                    return ()
        True  -> do s <- takeMVar v
                    s' <- case setPosition s (read p) of
                                Left msg -> do putStrLn msg
                                               return s
                                Right u  -> return u
                    putMVar v s'
      

readEvalPrintLoop :: MVar Simulation -> IO ()
readEvalPrintLoop v = 
    do printSimulation v
       e <- getLine
       let (continue, action) = case (words e) of
            ["HELP"]  -> (True,  printInstructions)
            ["QUIT"]  -> (False, putStrLn "QUITTING THE SIMULATION")
            ["START"] -> (True,  startSimulation v)
            ["STOP"]  -> (True,  stopSimulation v)
            ["REINIT"]-> (True,  reinitSimulation v)
            ["POS",n] -> (True,  setPositionSimulation n v)
            []        -> (True,  return ())
            _       -> (True,  putStrLn ("UNKNWON COMMAND:" ++ e))
       action
       updateSimulation v
       case continue of
        True  -> readEvalPrintLoop v
        False -> return ()

printReport :: MVar Simulation -> IO ()
printReport v =
    do s <- readMVar v 
       putStrLn (pretty (report s))

main = do printInstructions
          v <- newMVar newSimulation
          readEvalPrintLoop v
          printReport v
         
