module Main
where
import Happstack.Server (nullConf, simpleHTTP, toMessage, ok, lookText, ServerPart, Response)

server = do param <- lookText "param" 
            ok $ toMessage param

main :: IO ()
main = simpleHTTP nullConf server


