module Simulation
where
import RefrigeratedRoom

notRunning = "SIMULATION NOT RUNNING"

type States = [(Position,Temperature)]
data Simulation = Simulation {status :: Status, room :: RefrigeratedRoom, states :: States}
    deriving (Eq, Show)

data Status = Idle | Running | Stopped
    deriving (Eq, Show)

newSimulation :: Simulation
newSimulation = Simulation Idle newRoom []

start :: Simulation -> Simulation 
start s = s {status = Running}

roomInfo :: Simulation -> (Temperature, Position)
roomInfo s = (temperature (room s), position (room s))

setPosition :: Simulation -> Position -> Either String Simulation
setPosition s _ | status s /= Running = Left notRunning
setPosition s p = Right s {room = (room s) {position = p}}

updateRoom :: Simulation -> Either String Simulation
updateRoom s | status s /= Running = Left notRunning 
updateRoom s = Right s {room = update (room s), states = (position (room s),temperature (room s)):states s}

stop :: Simulation -> Simulation
stop s = s {status = Idle}

reinit :: Simulation -> Simulation
reinit _ = newSimulation
