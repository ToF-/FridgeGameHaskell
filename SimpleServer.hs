module Main
where
import Happstack.Server (nullConf, simpleHTTP, toMessage, ok, lookText)

main :: IO ()
main = simpleHTTP nullConf $ do param <- lookText "param" 
                                ok $ toMessage param

