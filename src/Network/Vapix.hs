{-# LANGUAGE OverloadedStrings #-}
module Network.Vapix where

import           Control.Lens
import           Network.URL
import           Network.Vapix.Internal
import           Network.Vapix.Types as Network.Vapix (AxisLogin(..), VapixRecording(..), unVapixInfo)
import           Network.Wreq
import           Text.XML.ToJSON
import Control.Monad.Catch


-- | list all current recordings on a camera/device
listRecordings :: AxisLogin -> IO [VapixRecording]
listRecordings login = do
  resp <- getWith opts url
  catch (views responseBody (\body -> (parseXML body) >>= (return . unVapixInfo)) resp) returnEmpty
  where opts = defaults & auth .~ (Just $ fromLogin login)
        url :: String
        url = exportURL $ addParams (buildURL urlHost "axis-cgi/record/list.cgi") [("startresultnumber", "0"), ("recordingid", "all")]
        urlHost = buildAxisHost login
        returnEmpty :: JSONParseError -> IO [VapixRecording]
        returnEmpty _ = return []

-- | Build the URL for a recording in order to download it
recordingURL :: AxisLogin -> VapixRecording -> String
recordingURL login vpr = url $ recordingId vpr 
  where url recId = exportURL (addParams (buildURL urlHost "axis-cgi/record/download.cgi") [("recordingid", recId)])
        urlHost = buildAxisHost login
