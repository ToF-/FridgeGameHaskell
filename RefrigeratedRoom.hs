module RefrigeratedRoom
where

data RefrigeratedRoom = Room { temperature :: Double, position :: Int }

newRoom :: RefrigeratedRoom 
newRoom = Room 15.0 100

update :: RefrigeratedRoom -> RefrigeratedRoom
update (Room t p) = Room (t-1) p
