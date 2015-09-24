{--
Important
[] Read defaults from template file (JSON)
[] Read data from input file (JSON)
[] Receive more than 1024 bytes
[] Split up in submodules

Nice to have
[] GUI
[] |- Data input: auto-generate a form using 
 --}
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
defaultHL7InFolder :: String
defaultHL7InFolder = "/Users/dvekeman/Documents/Middleware/_hl7/IN/UTF8"


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
          
mkSPM specimenId = SPM { specimenId = specimenId }
mkORC orderId orderTime = ORC { orderTime = orderTime
                              , orderId   = orderId
                              }
mkOBR universalServiceId = OBR { universalServiceId = universalServiceId }

fmtORCAndOBR orderId orderTime universalServiceIds =
  let formattedOBRs = map (fmtOBR . mkOBR) universalServiceIds
      orcs = map (\_ -> mkORC orderId orderTime) universalServiceIds
      formattedORCs = map fmtORC orcs
      formattedORCAndOBRZipped = zip formattedORCs formattedOBRs
      formattedORCAndOBRs = map (\(fst,snd) -> fst <> cr <> snd) formattedORCAndOBRZipped
  in  foldr (\formattedValue acc -> formattedValue <> cr <> acc) (B.byteString B.empty) formattedORCAndOBRs
  
fmtMSH :: MSH -> B.Builder
fmtMSH msh = B.stringUtf8 $ "MSH|^~\\&|||||" ++ (hl7Time $ dateTime msh) ++ "||OML^O33^OML_O33|603301|P|"++ (version msh) ++ "||||||" ++ (charset msh)

fmtSPM :: SPM -> B.Builder
fmtSPM spm = B.stringUtf8 $ "SPM||"++ (specimenId spm) ++"||FFPE"

fmtORC :: ORC -> B.Builder
fmtORC orc = B.stringUtf8 $ "ORC|NW|"++ (orderId orc) ++"|||||||" ++ (hl7Time $ orderTime orc)

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
main = do
  putStrLn "From file? (default: no)"
  useFile <- readS ""
  case useFile of
   "" -> fromScratch
   otherwise -> fromFile

fromScratch ::  IO ()
fromScratch = do
  now <- DT.getZonedTime
  putStrLn "Start Id (Order / Sample)"
  startId  <- readI 1
  putStrLn "Number of samples (default 1): "
  nrOfRequests  <- readI 1
  putStrLn "Universal Service Id(s): eg 101X,501X (default 101X)"
  univSvcIds  <- readS "101X"
  let univSvcIdList = wordsWhen (== ',') univSvcIds
  putStrLn "Order Time: format yyyymmddHHMMSS, eg 20150923115959 (default now)"
  orderTime <- readT now
  sequence_ $ map (fromScratchRequest univSvcIdList orderTime) (take nrOfRequests [startId..])

fromScratchRequest univSvcIdList orderTime startId = do
  now <- DT.getZonedTime
  let msh = fmtMSH $ mkMSH now
      spm = fmtSPM $ mkSPM ("S" ++ show startId)
      orcAndObr = fmtORCAndOBR ("O" ++ show startId) orderTime univSvcIdList
      requestData = msh <> cr <> spm <> cr <> orcAndObr
  sendRequest requestData

fromFile :: IO ()
fromFile = do
  putStrLn $ "File name (default path: " ++ defaultHL7InFolder ++ ")"
  fname <- readS "Blah.txt"
  requestData <- readFile (defaultHL7InFolder ++ "/" ++ fname)
  sendRequest (B.stringUtf8 requestData)

sendRequest requestData = do
  connect (hostname middleware) (show $ port middleware)  $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "Sending " ++ (show $ toBS requestData)
    let request     = sb <> requestData <> eb <> cr
    putStrLn $ "Raw hex data \n" ++ (prettyHex $ toBS request)
    send connectionSocket $ toBS $ request
    putStrLn "... waiting for response ..."
    response <- recv connectionSocket 1024
    case response of
     Just responseData -> do
                             putStrLn $ "Raw hex response data \n" ++ (prettyHex $ responseData)
                             putStrLn $ show $ prettyPrintHL7 responseData
     Nothing  -> putStrLn "|- Nothing received back -|"

readS :: String -> IO String
readS defaultValue = do
  value <- getLine
  putStrLn $ "Read: " ++ show value
  let result = sfoo defaultValue value
  putStrLn $ "Read result: " ++ show result
  return result

readI :: Int -> IO Int
readI defaultValue = do
  value <- getLine
  return $ foo defaultValue value

readB :: Bool -> IO Bool
readB defaultValue = do
  value <- getLine
  return $ foo defaultValue value

readT :: DT.ZonedTime -> IO DT.ZonedTime
readT defaultValue = do
  value <- getLine
  putStrLn $ "Value is: '" ++ value ++ "'"
  case value of
   "" -> do
     putStrLn "empty"
     return defaultValue
   otherwise -> do
     putStrLn "Not empty"
     return $ DT.parseTimeOrError True DT.defaultTimeLocale defaultTimeFormat value

sfoo :: String -> String -> String
sfoo defaultValue "" = defaultValue
sfoo _  s            = s
                    
foo :: Read a => a -> String -> a
foo defaultValue "" = defaultValue
foo _  s            = read s
                    
toBS :: B.Builder -> B.ByteString
toBS = BL.toStrict . B.toLazyByteString

prettyPrint :: B.ByteString -> String
prettyPrint = concat . map (flip showHex "") . B.unpack

prettyPrintHL7 :: B.ByteString -> B.ByteString
prettyPrintHL7 = B.takeWhile (\w -> w /= 0x1C) .  B.drop 1

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                    where (w, s'') = break p s'

infixr 4 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
