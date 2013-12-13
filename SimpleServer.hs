module Main
where
import Control.Monad (msum)
import Happstack.Server (nullConf, simpleHTTP, toMessage, ok, lookText, ServerPart, Response, dir, toResponse)
import qualified Text.Blaze.Html5 as H

server = msum [dir "serveparam" $ serveParam 
              ,dir "servepage"  $ servePage]

serveParam = do param <- lookText "param" 
                ok $ toResponse param

servePage = ok $ toResponse $
   H.html $ do
     H.head $ do
       H.title (H.toHtml "simple page")
     H.body $ do
       (H.toHtml "simple body")

main :: IO ()
main = simpleHTTP nullConf server


