module Main
where

printInstructions :: IO ()
printInstructions = putStrLn $ unlines
     ["REFRIGERATED ROOM SIMULATION"
     ,"START <n> : start the simulator for n turns"
     ,"POS   <n> : set the position [0..100] for the command"
     ,"<empty>   : let the simulator unchanged"
     ,"STOP      : stop the simulator and quit"
     ,"REINIT    : put the simulator back to initial state"]

main = do printInstructions
