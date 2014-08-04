{-# LANGUAGE OverloadedStrings #-}
module Network.Vapix where

import           Control.Applicative
import           Control.Lens
import           Data.ByteString
import           Data.Time.Clock
import           Network.Vapix.Internal
import           Network.Vapix.Types
import           Network.Wreq
import           Text.XML.ToJSON
import           Data.ByteString.Char8 as BSC
--listRecordings :: String -> IO [VapixRecording]
--listRecordings server = do
--  res <- get server
--  case res of
--    (Success res) -> parseXML res
--    (Error e) -> fail . show $ e

listRecordings :: AxisLogin -> String -> IO [VapixRecording]
listRecordings login server = do
  resp <- getWith opts server
  views responseBody (\body -> (parseXML body) >>= (return . unVapixInfo)) resp
  where opts = defaults & auth .~ fromLogin login


--buildRecordingURL :: AxisLogin

fetchRecording :: AxisLogin ->  String -> VapixRecording -> IO [VapixRecording]
fetchRecording login server r = do
  resp <- getWith opts server
  print resp
  views responseBody (\body -> (parseXML body) >>= (return . unVapixInfo)) resp
  where opts = defaults & auth .~ fromLogin login 

fromLogin :: AxisLogin -> Maybe Auth
fromLogin (AxisLogin user pass) = basicAuth (BSC.pack user) (BSC.pack pass)


--asXML resp = resp ^? responseBody . parseXML
