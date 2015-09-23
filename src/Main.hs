{-# LANGUAGE OverloadedStrings #-}

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S
import Network.Http.Client

main :: IO ()
main = do
  c <- openConnection "www.example.com" 80
  
  let q = buildRequest1 $ do
        http GET "/"
        setAccept "text/html"
  
  sendRequest c q emptyBody
  
  receiveResponse c (\p i -> do
                        xm <- Streams.read i
                        case xm of
                         Just x    -> S.putStr x
                         Nothing   -> "")
  closeConnection c
