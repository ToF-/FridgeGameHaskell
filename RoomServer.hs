module RoomServer
where
import RefrigeratedRoom

data RoomServer = RoomServer { status :: Status, room :: RefrigeratedRoom }
    deriving (Eq, Show)

data Status = Idle | Running | Stopped
    deriving (Eq, Show)

newServer :: RoomServer
newServer = RoomServer Idle newRoom

start :: RoomServer -> RoomServer 
start s = s { status = Running }

roomInfo :: RoomServer -> (Double, Int)
roomInfo s = (temperature (room s), position (room s))

setPosition :: RoomServer -> Int -> RoomServer
setPosition s p = let r = room s in s { room = r { position = p } }

updateRoom :: RoomServer -> RoomServer
updateRoom s = let r = room s in s { room = update r }

stop :: RoomServer -> RoomServer
stop s = s { status = Idle }

reinit :: RoomServer -> RoomServer
reinit _ = newServer
