module Report
where
import RefrigeratedRoom
import Simulation

type Report = [(Int, Position, Temperature)]

report :: Simulation -> Report
report s = [(n,p,t) | (n,(p,t)) <- zip [1..] (reverse (states s))]

pretty :: Report -> String
pretty s = unlines [(show n) ++ "\t" ++ (show p) ++ "\t" ++ (show t) | (n,p,t) <- s] 

showSimulation :: Simulation -> String
showSimulation s = "STATUS:"++(show (status s))
                    ++" POSITION:"++(show (position (room s)))
                    ++" TEMPERATURE:" ++ (show (rounded (temperature (room s))))
                    where rounded n = fromIntegral (round (n * 10)) / 10.0  
