module Main
where
import RefrigeratedRoom
import Simulation
import Report

process :: Int -> Int -> Simulation -> IO ()
process _ 0 s = do putStrLn "SIMULATION COMPLETE"
                   putStrLn (pretty $ report s)

process max n s = do putStrLn $ (show (position (room s))) ++ " " ++ (show (temperature (room s)))
                     putStrLn "COMMAND: (0..100 / STOP / REINIT)"
                     entry <- getLine
                     let (n',s') = case entry of 
                                    "START" -> (max, start newSimulation)
                                    "STOP" -> (0, stop s)
                                    "REINIT" -> (max, newSimulation)
                                    n -> ((read n)-1, case setPosition s (read n) of
                                                        Left m -> s
                                                        Right x-> x)
                     case updateRoom s' of
                         Left m -> do putStrLn m
                                      process max n' s'
                         Right s' -> process max (n'-1) s'

main = do putStrLn "SIMULATION.. CTL-C TO QUIT"
          process 20 20 $ start newSimulation
