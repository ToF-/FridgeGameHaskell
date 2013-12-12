module Main
where
import Happstack.Server (nullConf, simpleHTTP, toMessage, ok)

main :: IO ()
main = simpleHTTP nullConf $ ok $ toMessage "42"

