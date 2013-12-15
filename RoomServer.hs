module RoomServer
where
import RefrigeratedRoom

notRunning = "SERVER NOT RUNNING"

data RoomServer = RoomServer {status :: Status, room :: RefrigeratedRoom}
    deriving (Eq, Show)

data Status = Idle | Running | Stopped
    deriving (Eq, Show)

newServer :: RoomServer
newServer = RoomServer Idle newRoom

start :: RoomServer -> RoomServer 
start s = s {status = Running}

roomInfo :: RoomServer -> (Double, Int)
roomInfo s = (temperature (room s), position (room s))

setPosition :: RoomServer -> Int -> Either String RoomServer
setPosition s _ | status s /= Running = Left notRunning
setPosition s p = Right s {room = (room s) {position = p}}

updateRoom :: RoomServer -> Either String RoomServer
updateRoom s | status s /= Running = Left notRunning 
updateRoom s = Right s {room = update (room s)}

stop :: RoomServer -> RoomServer
stop s = s {status = Idle}

reinit :: RoomServer -> RoomServer
reinit _ = newServer
