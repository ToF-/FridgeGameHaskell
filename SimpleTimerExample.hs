module Main
where

import Control.Concurrent.Timer (newTimer, repeatedStart, stopTimer)
import Control.Concurrent.Suspend.Lifted (Delay, sDelay)

action :: IO ()
action = putStrLn "ACTION!"

delay :: Delay
delay = sDelay 2

main = do t <- newTimer
          repeatedStart t action delay
          putStrLn "type Enter to stop the timer..."
          getLine
          stopTimer t
        
