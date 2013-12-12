{-# LANGUAGE DeriveDataTypeable #-}
module Main
where
import Happstack.Server (nullConf, simpleHTTP, toResponse, ok)
import Text.JSON.Generic

main :: IO ()
main = simpleHTTP nullConf $ ok $ toResponse "42"

