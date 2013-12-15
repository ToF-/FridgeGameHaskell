module Main
where
import Simulation
import Report

main = do putStrLn "SIMULATION.. CTL-C TO QUIT"
          let sim = newSimulation
              sts = recordState sim []
          putStrLn (show (report sts))
