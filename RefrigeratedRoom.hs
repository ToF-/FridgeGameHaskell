module RefrigeratedRoom
where

data RefrigeratedRoom = Room

newRoom :: RefrigeratedRoom
newRoom = Room

temperature :: RefrigeratedRoom -> Double
temperature _ = 15.0
