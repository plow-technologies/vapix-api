{-# LANGUAGE OverloadedStrings #-}
module Network.Vapix where

import           Control.Applicative
import           Control.Lens
import           Data.ByteString
import           Data.ByteString.Char8  as BSC
import           Data.Time.Clock
import           Network.URL
import           Network.Vapix.Internal
import           Network.Vapix.Types
import           Network.Wreq
import           Text.XML.ToJSON
--listRecordings :: String -> IO [VapixRecording]
--listRecordings server = do
--  res <- get server
--  case res of
--    (Success res) -> parseXML res
--    (Error e) -> fail . show $ e

listRecordings :: AxisLogin -> IO [VapixRecording]
listRecordings login = do
  Prelude.putStrLn url
  resp <- getWith opts url
  views responseBody (\body -> (parseXML body) >>= (return . unVapixInfo)) resp
  where opts = defaults & auth .~ fromLogin login
        url :: String
        url = exportURL $ addParams (buildURL urlHost "axis-cgi/record/list.cgi") [("startresultnumber", show 0), ("recordingid", "all")]
        urlHost = buildAxisHost login

--buildRecordingURL :: AxisLogin

fetchRecording :: AxisLogin -> VapixRecording -> String
fetchRecording login vpr = url $ recordingId vpr 
  where opts = defaults & auth .~ fromLogin login
        url recId = exportURL (addParams (buildURL urlHost "axis-cgi/record/download.cgi") [("recordingid", recId)])
        urlHost = buildAxisHost login



fromLogin :: AxisLogin -> Maybe Auth
fromLogin (AxisLogin user pass h p) = basicAuth (BSC.pack user) (BSC.pack pass)

--http://192.168.88.254/axis-cgi/record/download.cgi?recordingid=20140804_145959_6EAB_00408CFB5CD9&timestamp=1407186629706


--asXML resp = resp ^? responseBody . parseXML

buildAxisHost :: AxisLogin -> Host
buildAxisHost login = buildHost (axisHost login) (Just . fromIntegral . axisPort $ login)

buildHost :: String -> Maybe Integer -> Host
buildHost h p = Host (HTTP False) h p

buildURL :: Host -> String -> URL
buildURL urlHost urlPath = URL (Absolute urlHost) urlPath []

addParams :: URL -> [(String, String)] -> URL
addParams url urlParams = Prelude.foldl (\u p -> add_param u p) url urlParams