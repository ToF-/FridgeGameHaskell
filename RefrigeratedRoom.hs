module RefrigeratedRoom
where

type Temperature = Double
type Position = Int

data RefrigeratedRoom = Room { temperatures :: [Temperature], position :: Position }
    deriving (Show, Eq)

temperature :: RefrigeratedRoom -> Temperature
temperature = head . temperatures

initialTemperatures :: [Temperature]
initialTemperatures = take 5 $ repeat 15

newRoom :: RefrigeratedRoom 
newRoom = Room initialTemperatures 100

update :: RefrigeratedRoom -> RefrigeratedRoom
update r = let 
        t = temperature r
        ts = temperatures r
        p  = fromIntegral (position r)
        delta = (p / 10.0 + 2.0 - ts !! 4) / 3.0
    in r { temperatures = (t+delta):ts }
