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
                          putStrLn $ displaySimulationState s



setPosSimulation :: String -> Runner -> Id -> IO (Either String Simulation)
setPosSimulation p r id = setPositionSimulation r id p

execute :: IO (Either String Simulation) -> IO ()
execute a = do result <- a 
               case result of
                 Left msg -> putStrLn msg
                 _ -> return ()


readEvalPrintLoop :: Runner -> Id -> IO ()
readEvalPrintLoop r id = 
    do printSimulation r id
       e <- getLine
       let (continue, command) = case (map (map toUpper) (words e)) of
            ["HELP"]  -> (True,  printInstructions)
            ["QUIT"]  -> (False, putStrLn "QUITTING THE SIMULATION")
            ["START"] -> (True,  execute (action start r id))
            ["STOP"]  -> (True,  execute (action stop r id))
            ["REINIT"]-> (True,  execute (action reinit r id))
            ["POS",n] -> (True,  execute (setPosSimulation n r id))
            []        -> (True,  return ())
            _       -> (True,  putStrLn ("UNKNWON COMMAND:" ++ e))
       command
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

find r id = do s <- action retrieve r id
               return (fromRight s)

tickSimulation :: Runner -> Id -> IO ()
tickSimulation r id =
    do action updateRoom r id
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
          register r id
          t <- newTimer
          repeatedStart t (tickSimulation r id) d
          readEvalPrintLoop r id
          printReport r id
          exitSuccess
