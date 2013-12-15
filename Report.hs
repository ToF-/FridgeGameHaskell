module Report
where
import RefrigeratedRoom
import Simulation

type Report = [(Int, Position, Temperature)]

report :: Simulation -> Report
report s = [(n,p,t) | (n,(p,t)) <- zip [1..] (reverse (states s))]


