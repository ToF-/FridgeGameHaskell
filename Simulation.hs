module Simulation
where
import RefrigeratedRoom

notRunning = "SIMULATION NOT RUNNING"
illegalRange = "POSITION SHOULD BE WITHIN RANGE [0..100]"

type States = [(Position,Temperature)]
data Simulation = Simulation {status :: Status, room :: RefrigeratedRoom, states :: States}
    deriving (Eq, Show)

data Status = Idle | Running | Stopped
    deriving (Eq, Show)

newSimulation :: Simulation
newSimulation = Simulation Idle newRoom []

start :: Simulation -> Either String Simulation 
start s = Right s {status = Running}

roomInfo :: Simulation -> (Temperature, Position)
roomInfo s = (temperature (room s), position (room s))

setPosition :: Position -> Simulation -> Either String Simulation
setPosition _ s | status s /= Running = Left notRunning
setPosition p s | p < 0 = Left illegalRange
setPosition p s | p > 100 = Left illegalRange
setPosition p s = Right s {room = (room s) {position = p}}

updateRoom :: Simulation -> Either String Simulation
updateRoom s | status s /= Running = Left notRunning 
updateRoom s = Right s {room = update (room s), states = (position (room s),temperature (room s)):states s}

stop :: Simulation -> Either String Simulation
stop s = Right s {status = Idle}

reinit :: Simulation -> Either String Simulation
reinit _ = Right newSimulation
