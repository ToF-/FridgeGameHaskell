module Report
where
import RefrigeratedRoom
import RoomServer

type States = [(Position,Temperature)]
type Report = [(Int, Position, Temperature)]

recordState :: RoomServer -> States -> States
recordState s ss = (position r, temperature r):ss where r = room s

report :: States -> Report
report ss = [(n,p,t) | (n,(p,t)) <- zip [1..] (reverse ss)]


