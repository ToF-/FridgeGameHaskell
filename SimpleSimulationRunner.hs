module Main
where
import RefrigeratedRoom
import Simulation
import Report

process :: Int -> Simulation -> IO ()
process 0 s = do putStrLn "SIMULATION COMPLETE"
                 putStrLn (show (report s))

process n s = do putStrLn $ (show (position (room s))) ++ " " ++ (show (temperature (room s)))
                 case updateRoom s of
                    Left m -> do putStrLn m
                                 process n s
                    Right s' -> process (n-1) s'

main = do putStrLn "SIMULATION.. CTL-C TO QUIT"
          process 20 $ start newSimulation
