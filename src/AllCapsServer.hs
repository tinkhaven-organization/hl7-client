{-# LANGUAGE OverloadedStrings #-}
import           Conduit
import           Data.Conduit.Network
import qualified Data.Word8           as W8 (Word8, toUpper)
import           Codec.Binary.UTF8.String (encode, decode)

main :: IO ()
main = runTCPServer (serverSettings 4000 "*") $ \appData ->
  appSource appData $$ omapCE W8.toUpper =$ appSink appData

