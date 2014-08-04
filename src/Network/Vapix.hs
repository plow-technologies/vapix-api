module Network.Vapix where

import Network.Vapix.Internal
import Data.Time.Clock

data Recording = Recording {
  recordingDisk :: String
, recordingId :: String
, recordingStart :: UTCTime
, recordingEnd :: UTCTime
, recordingType :: String
} deriving (Eq)

listRecordings :: String -> [Recording]
listRecordings server = undefined

fetchRecording :: String -> Recording -> IO ()
fetchRecording = undefined