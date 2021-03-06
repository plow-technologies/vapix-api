{-# LANGUAGE OverloadedStrings, FlexibleInstances,RecordWildCards #-}
module Network.Vapix.Types where

import           Control.Applicative
import           Data.Aeson
import           Data.Time.Clock
import Data.Time.Format
import System.Locale
import Data.Text

-- | The username, password, host (just IP/host name "192.168.1.7" or "somewhere.example.com"), and the port to connect through
data AxisLogin = AxisLogin {
  axisUsername :: String
, axisPassword :: String
, axisHost :: String
, axisPort :: Int
} deriving (Show, Eq)

data VapixXML = VapixXML {
  vapixRoot :: VapixRecordingsWrapper
} deriving (Show, Eq)

data VapixRecordingsWrapper = VapixRecordingsWrapper {
  vapixRecordingsWrapper :: VapixRecordings
} deriving (Eq, Show)

data VapixRecordings = VapixRecordingsList { vapixRecordingList :: [VapixRecordingsAttr] } | VapixRecordingsSingle { vapixRecordingSingle :: VapixRecordingsAttr } deriving (Eq, Show)

data VapixRecordingsAttr = VapixRecordingsAttr {
  vapixRecordingsAttr :: VapixRecording
} deriving (Eq, Show)

-- | A specific recording. All are data types from the vapix XML
data VapixRecording = VapixRecording {
  recordingDisk  :: String
, recordingId    :: String
, recordingStart :: VapixDate
, recordingEnd   :: Maybe VapixDate
, recordingType  :: String
} deriving (Eq, Show)

unVapixInfo :: VapixXML -> [VapixRecording]
unVapixInfo = fmap vapixRecordingsAttr . unVapixRecordings . vapixRecordingsWrapper . vapixRoot

unVapixRecordings :: VapixRecordings -> [VapixRecordingsAttr]
unVapixRecordings (VapixRecordingsList xs) = xs
unVapixRecordings (VapixRecordingsSingle x) = [x]

newtype VapixDate = VapixDate { unVapixDate :: UTCTime } deriving (Eq,Ord, Show)

instance FromJSON VapixDate where
  parseJSON (String s) = case unpack s of
                          "" -> fail "Unable to parse empty string"
                          unS -> return $ VapixDate $ readTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" unS
  parseJSON _ = fail "Rule: Expecting VapixStartDate received other"

instance ToJSON VapixDate where
  toJSON (VapixDate {..}) = object [
                               "unVapixDate" .= unVapixDate
                               ]

instance FromJSON (Maybe VapixDate) where
  parseJSON (String s) = case unpack s of
                          "" -> return Nothing
                          unS -> return . Just $ VapixDate $ readTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" unS
  parseJSON _ = fail "Rule: Expecting VapixStartDate received other"


instance FromJSON VapixXML where
  parseJSON (Object tObj) = VapixXML <$> tObj .: "root"
  parseJSON _ = fail "Rule: Expecting VapixXML received other"

instance FromJSON VapixRecordingsWrapper where
  parseJSON (Object tObj) = VapixRecordingsWrapper <$> tObj .: "recordings"
  parseJSON _ = fail "Rule: Expecting VapixRecordingsWrapper received other"
 
instance FromJSON VapixRecordingsAttr where
  parseJSON (Object tObj) = VapixRecordingsAttr <$> tObj .: "__attributes"
  parseJSON _ = fail "Rule: Expecting VapixRecording attributes, received other"

instance FromJSON VapixRecordings where
  parseJSON (Object tObj) = (VapixRecordingsList <$> tObj .: "recording") <|> (VapixRecordingsSingle <$> tObj .: "recording")
  parseJSON _ = fail "Rule: Expecting VapixRecordings received other"


instance FromJSON VapixRecording where
  parseJSON (Object tObj) = VapixRecording <$> tObj .: "diskid"
                                           <*> tObj .: "recordingid"
                                           <*> tObj .: "starttime"
                                           <*> tObj .: "stoptime"
                                           <*> tObj .: "recordingtype"
  parseJSON _ = fail "Rule: Expecting VapixRecording received other"

instance ToJSON VapixRecording where
  toJSON (VapixRecording {..}) = object [
                                    "recordingDisk" .= recordingDisk
                                   ,"recordingId" .= recordingId
                                   ,"recordingStart" .= recordingStart
                                   ,"recordingEnd" .= recordingEnd
                                   ,"recordingType" .= recordingType
                                    ]
