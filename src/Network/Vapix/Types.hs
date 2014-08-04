{-# LANGUAGE OverloadedStrings #-}
module Network.Vapix.Types where

import           Control.Applicative
import           Data.Aeson
import           Data.Time.Clock

data VapixXML = VapixXML {
  vapixRoot :: VapixRecordingsWrapper
} deriving (Show, Eq)

data VapixRecordingsWrapper = VapixRecordingsWrapper {
  vapixRecordingsWrapper :: VapixRecordings
} deriving (Eq, Show)

data VapixRecordings = VapixRecordings {
  vapixRecordings :: [VapixRecording]
} deriving (Eq, Show)

data VapixRecording = VapixRecording {
  recordingDisk  :: String
, recordingId    :: String
, recordingStart :: UTCTime
, recordingEnd   :: Maybe UTCTime
, recordingType  :: String
} deriving (Eq, Show)

instance FromJSON VapixXML where
  parseJSON (Object tObj) = VapixXML <$> tObj .: "root"
  parseJSON _ = fail "Rule: Expecting VapixXML received other"

instance FromJSON VapixRecordingsWrapper where
  parseJSON (Object tObj) = VapixRecordingsWrapper <$> tObj .: "recordings"
  parseJSON _ = fail "Rule: Expecting VapixRecordingsWrapper received other"
 

instance FromJSON VapixRecordings where
  parseJSON (Object tObj) = VapixRecordings <$> tObj .: "recording"
  parseJSON _ = fail "Rule: Expecting VapixRecordings received other"


instance FromJSON VapixRecording where
  parseJSON (Object tObj) = VapixRecording <$> tObj .: "diskid"
                                           <*> tObj .: "recordingid"
                                           <*> tObj .: "starttimelocal"
                                           <*> tObj .: "stoptimelocal"
                                           <*> tObj .: "recordingtype"
  parseJSON _ = fail "Rule: Expecting VapixRecording received other"