module Main
where
import RefrigeratedRoom
import Simulation (newSimulation, start, stop, room, setPosition, updateRoom, Simulation)
import Report

showStatus :: Simulation -> String
showStatus s = (show ((position . room) s)) ++ " " ++ (show ((temperature . room) s))

process :: Int -> Int -> Either String Simulation -> IO ()
process _ 0 (Right s) = do
    putStrLn "SIMULATION COMPLETE"
    putStrLn (pretty $ report s)

process max n (Right s) = do 
    putStrLn (showStatus s)
    putStrLn "COMMAND: (0..100 / STOP / REINIT)"
    entry <- getLine
    let (n',Right s') = case entry of 
                    "START" -> (max, start newSimulation)
                    "STOP" -> (0, stop s)
                    "REINIT" -> (max, Right newSimulation)
                    n -> ((read n)-1, case setPosition (read n) s of
                                        Left m -> Right s
                                        Right x-> Right x)

    case updateRoom s' of
        Left m -> do 
            putStrLn m
            process max n' (Right s')
        Right s' -> process max (n'-1) (Right s')

main = do putStrLn "SIMULATION.. CTL-C TO QUIT"
          process 20 20 $ start newSimulation
