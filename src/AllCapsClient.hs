{-# LANGUAGE OverloadedStrings #-}

import           Conduit
import           Control.Concurrent.Async (concurrently)
import           Control.Monad            (void)
import           Data.Conduit.Network

-- http://www.yesodweb.com/blog/2014/03/network-conduit-async

main :: IO ()
main =
  runTCPClient (clientSettings 4000 "127.0.0.1") $ \server ->
  void $ concurrently
      (stdinC $$ appSink server)
      (appSource server $$ stdoutC)
                          
