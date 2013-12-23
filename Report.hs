module Report
where
import RefrigeratedRoom
import Simulation

type Report = [(Int, Position, Temperature)]

report :: Simulation -> Report
report s = [(n,p,t) | (n,(p,t)) <- zip [1..] (reverse (history s))]

pretty :: Report -> String
pretty s = unlines [(show n) ++ "\t" ++ (show p) ++ "\t" ++ (show t) | (n,p,t) <- s] 

showSimulation :: Simulation -> (String, String, String)
showSimulation s = (show $ status s, 
                    show $ position $ room s, 
                    show $ rounded $ temperature $room s)
                    where rounded n = fromIntegral (round (n * 10)) / 10.0  

displaySimulationState :: Simulation -> String
displaySimulationState s = display  "STATUS"      sts ++ 
                           display " POSITION"    pos ++
                           display " TEMPERATURE" tmp 
                           where display l v = l ++ ":" ++ v
                                 (sts,pos,tmp) = showSimulation s
