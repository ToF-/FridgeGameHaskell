module Main
where
import Control.Monad (msum)
import Happstack.Lite
import Data.Text.Lazy (unpack)
-- import Happstack.Server (nullConf, simpleHTTP, toMessage, ok, lookText, ServerPart, Response, dir, toResponse)
import qualified Text.Blaze.Html5 as H

server = msum [dir "serveparam" $ serveParam 
              ,dir "servepage"  $ servePage
              ,dir "compute"    $ serveComputation
              ,serveFiles]

serveParam = do param <- lookText "param" 
                ok $ toResponse param

serveComputation = do param <- lookText "param"
                      let value = read $ unpack param
                      ok $ toResponse (show (value * value))

servePage = ok $ toResponse $
   H.html $ do
     H.head $ do
       H.title (H.toHtml "simple page")
     H.body $ do
       (H.toHtml "simple body")

serveFiles = serveDirectory EnableBrowsing ["index.html"] "."

main :: IO ()
main = serve Nothing server


