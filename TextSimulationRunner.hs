module Main
where
import Data.Char
import System.Environment
import System.Exit
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
     ,"POS   <n> : set the position [0..200] for the command"
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

setPosSimulation :: String -> Runner -> Id -> IO (Either String Simulation)
setPosSimulation p r id = 
    case isInteger p of
        False -> return (Left ("NOT AN INTEGER:" ++ p))
        True  -> setPositionSimulation r id (read p)

execute :: IO (Either String Simulation) -> IO ()
execute a = do result <- a 
               case result of
                 Left msg -> putStrLn msg
                 _ -> return ()


readEvalPrintLoop :: Runner -> Id -> IO ()
readEvalPrintLoop r id = 
    do printSimulation r id
       e <- getLine
       let (continue, action) = case (map (map toUpper) (words e)) of
            ["HELP"]  -> (True,  printInstructions)
            ["QUIT"]  -> (False, putStrLn "QUITTING THE SIMULATION")
            ["START"] -> (True,  execute (startSimulation r id))
            ["STOP"]  -> (True,  execute (stopSimulation r id))
            ["REINIT"]-> (True,  execute (reinitSimulation r id))
            ["POS",n] -> (True,  execute (setPosSimulation n r id))
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
       writeFile ("STATS_" ++ id ++ ".txt") (pretty (report s))

delay :: Delay
delay = sDelay 5

find r id = do s <- retrieve r id
               return (fromRight s)

tickSimulation :: Runner -> Id -> IO ()
tickSimulation r id =
    do updateSimulation r id
       printSimulation r id

validateArgs :: [String] -> IO ()
validateArgs ss 
   | length ss == 2 && isInteger (ss !! 0) = return ()
validateArgs _   = do putStrLn "USAGE: FRIDGE <delay in seconds> <player name>"
                      exitWith (ExitFailure 1)
                  
main = do args <- getArgs
          validateArgs args 
          let d = sDelay (read (args !! 0))
          let id = args !! 1
          printInstructions
          r <- newRunner
          register r id newSimulation
          t <- newTimer
          repeatedStart t (tickSimulation r id) d
          readEvalPrintLoop r id
          printReport r id
          exitSuccess
          

        
