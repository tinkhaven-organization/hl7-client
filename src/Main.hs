{-# LANGUAGE OverloadedStrings #-}

import           Network.Simple.TCP
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Builder as B
import qualified Data.Time.Clock         as DT
import qualified Data.Time.LocalTime     as DT
import qualified Data.Time.Format        as DT
-- import           System.Locale                 (defaultTimeLocale)
import           Numeric                       (showHex)
import           Hexdump

-- stringUtf8 :: String -> Builder

{--
# Time
http://tab.snarc.org/posts/haskell/2011-12-16-date-in-haskell.html
## Data.Time.Clock.POSIX
posixSecondsToUTCTime and utcTimeToPOSIXSeconds.

##  Data.Time.LocalTime
utcToZonedTime and zonedTimeToUTC
getCurrentTime :: IO UTCTime
getCurrentTimeZone :: IO TimeZone
getZonedTime :: IO ZonedTime

## Data.Time.Format
parseTime and formatTime
_You need to import System.Locale from the old-locale package to get the defaultTimeLocale_
--}

type CharacterSet = String

utf8 :: CharacterSet
utf8 = "UNICODE UTF-8"
latin1 :: CharacterSet
latin1 = "8895/1"
ascii :: CharacterSet
ascii = "ASCII"

type Version = String
v251 = "2.5.1" :: Version

data MSH = MSH { dateTime :: DT.ZonedTime
               , version  :: Version
               , charset  :: CharacterSet
               } deriving Show
data SPM = SPM { specimenId :: String
               }
data ORC = ORC { orderId :: String
               , orderTime :: DT.ZonedTime
               }
data OBR = OBR { universalServiceId :: String
               }

mkMSH t = MSH { dateTime = t
              , version  = v251
              , charset  = utf8
              }
          
fmtMSH :: MSH -> B.Builder
fmtMSH msh = B.stringUtf8 $ "MSH|^~\\&|||||" ++ (hl7Time $ dateTime msh) ++ "||OML^O33^OML_O33|603301|P|"++ (version msh) ++ "||||||" ++ (charset msh)

mkSPM specimenId = SPM { specimenId = specimenId
                       }
fmtSPM :: SPM -> B.Builder
fmtSPM spm = B.stringUtf8 $ "SPM||"++ (specimenId spm) ++"||FFPE"

mkORC orderId orderTime = ORC { orderId = orderId
                              , orderTime = orderTime
                              }
fmtORC :: ORC -> B.Builder
fmtORC orc = B.stringUtf8 $ "ORC|NW|"++ (orderId orc) ++"|||||||" ++ (hl7Time $ orderTime orc)

mkOBR universalServiceId = OBR { universalServiceId = universalServiceId
                               }
fmtOBR :: OBR -> B.Builder
fmtOBR obr = B.stringUtf8 $ "OBR||||" ++ (universalServiceId obr)

defaultTimeFormat :: String
defaultTimeFormat = "%Y%m%d%H%M%S"

hl7Time :: DT.ZonedTime -> String
hl7Time = DT.formatTime DT.defaultTimeLocale defaultTimeFormat

cr :: B.Builder
cr = B.word8 13

lf :: B.Builder
lf = B.word8 10

sb :: B.Builder
sb = B.word8 0x0B

eb :: B.Builder
eb = B.word8 0x1C

data Settings = Settings { hostname :: String
                         , port     :: Int
                         }

middleware :: Settings
middleware = Settings "127.0.0.1" 2575

main :: IO ()
main =
  connect (hostname middleware) (show $ port middleware)  $ \(connectionSocket, remoteAddr) -> do
    now <- DT.getZonedTime
    putStrLn "Connecting ..."
    putStrLn "Start Id (Order / Sample)"
    startId  <- readI 1
    putStrLn "Order ID: (default )"
    orderId  <- readS ("O" ++ show startId)
    putStrLn "Sample ID: (default )"
    sampleId  <- readS ("S" ++ show startId)
    putStrLn "Universal Service Id: (default 101X)"
    univSvcId  <- readS "101X"
    putStrLn "Order Time (default now, format yyyymmddHHMMSS, eg 20150923115959)"
    orderTime <- readT now
    putStrLn $ "Order ID: " ++ (show orderId)
    putStrLn $ "Sample ID: " ++ (show sampleId)
    let msh = fmtMSH $ mkMSH now
        spm = fmtSPM $ mkSPM sampleId
        orc = fmtORC $ mkORC orderId orderTime
        obr = fmtOBR $ mkOBR univSvcId
    putStrLn $ "Connection established to " ++ show remoteAddr
    let requestData = msh <> cr <> spm <> cr <> orc <> cr <> obr
    let request     = sb <> requestData <> eb <> cr
    putStrLn $ "Sending " ++ (show $ toBS requestData)
    putStrLn $ "Raw hex data \n" ++ (prettyHex $ toBS request)
    send connectionSocket $ toBS $ request
    putStrLn "... waiting for response ..."
    response <- recv connectionSocket 1024
    case response of
     Just responseData -> do
                             putStrLn $ "Raw hex response data \n" ++ (prettyHex $ responseData)
                             putStrLn $ show $ prettyPrintHL7 responseData
     Nothing  -> putStrLn "|- Nothing received back -|"
      -- Now you may use connectionSocket as you please within this scope,
      -- possibly using recv and send to interact with the remote end.

readS :: String -> IO String
readS defaultValue = do
  value <- getLine
  return $ foo defaultValue value

readI :: Int -> IO Int
readI defaultValue = do
  value <- getLine
  return $ foo defaultValue value

readT :: DT.ZonedTime -> IO DT.ZonedTime
readT defaultValue = do
  value <- getLine
  let valueT = DT.parseTimeOrError True DT.defaultTimeLocale defaultTimeFormat value :: DT.ZonedTime
  return valueT

foo :: Read a => a -> String -> a
foo defaultValue "" = defaultValue
foo _  s            = read s
                    
toBS :: B.Builder -> B.ByteString
toBS = BL.toStrict . B.toLazyByteString

prettyPrint :: B.ByteString -> String
prettyPrint = concat . map (flip showHex "") . B.unpack

prettyPrintHL7 :: B.ByteString -> B.ByteString
prettyPrintHL7 = B.takeWhile (\w -> w /= 0x1C) .  B.drop 1

infixr 4 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
