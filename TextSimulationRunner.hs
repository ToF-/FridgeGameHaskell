module Main
where
import Data.Char
import Simulation
import Report
import Runner
import Control.Concurrent.Timer (newTimer, repeatedStart, stopTimer)
import Control.Concurrent.Suspend.Lifted (Delay, sDelay)

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "fromRight applied to a Left value"

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

printSimulation :: Runner -> Id -> IO ()
printSimulation r id = do s <- find r id
                          putStrLn $ showSimulation s


isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

setPosSimulation :: String -> Runner -> Id -> IO ()
setPosSimulation p r id = 
    case isInteger p of
        False -> do putStrLn ("NOT AN INTEGER:" ++ p)
                    return ()
        True  -> do setPositionSimulation r id (read p)

readEvalPrintLoop :: Runner -> Id -> IO ()
readEvalPrintLoop r id = 
    do printSimulation r id
       e <- getLine
       let (continue, action) = case (map (map toUpper) (words e)) of
            ["HELP"]  -> (True,  printInstructions)
            ["QUIT"]  -> (False, putStrLn "QUITTING THE SIMULATION")
            ["START"] -> (True,  startSimulation r id)
            ["STOP"]  -> (True,  stopSimulation r id)
            ["REINIT"]-> (True,  reinitSimulation r id)
            ["POS",n] -> (True,  setPosSimulation n r id)
            []        -> (True,  return ())
            _       -> (True,  putStrLn ("UNKNWON COMMAND:" ++ e))
       action
       case continue of
        True  -> readEvalPrintLoop r id
        False -> return ()

printReport :: Runner -> Id -> IO ()
printReport r id =
    do s <- find r id
       putStrLn (pretty (report s))

delay :: Delay
delay = sDelay 5

find r id = do s <- retrieve r id
               return (fromRight s)

tickSimulation :: Runner -> Id -> IO ()
tickSimulation r id =
    do updateSimulation r id
       printSimulation r id

main = do printInstructions
          r <- newRunner
          register r "TOF" newSimulation
          t <- newTimer
          repeatedStart t (tickSimulation r "TOF") delay
          readEvalPrintLoop r "TOF"
          printReport r "TOF"
        
