{-# LANGUAGE OverloadedStrings #-}
module Network.Vapix where

import           Control.Lens
import           Network.URL
import           Network.Vapix.Internal
import           Network.Vapix.Types as Network.Vapix (AxisLogin(..), VapixRecording(..), unVapixInfo)
import           Network.Wreq
import           Text.XML.ToJSON


-- | list all current recordings on a camera/device
listRecordings :: AxisLogin -> IO [VapixRecording]
listRecordings login = do
  resp <- getWith opts url
  views responseBody (\body -> (parseXML body) >>= (return . unVapixInfo)) resp
  where opts = defaults & auth .~ fromLogin login
        url :: String
        url = exportURL $ addParams (buildURL urlHost "axis-cgi/record/list.cgi") [("startresultnumber", "0"), ("recordingid", "all")]
        urlHost = buildAxisHost login

-- | Build the URL for a recording in order to download it
recordingURL :: AxisLogin -> VapixRecording -> String
recordingURL login vpr = url $ recordingId vpr 
  where url recId = exportURL (addParams (buildURL urlHost "axis-cgi/record/download.cgi") [("recordingid", recId)])
        urlHost = buildAxisHost login
