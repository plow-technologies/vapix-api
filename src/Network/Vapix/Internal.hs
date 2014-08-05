module Network.Vapix.Internal
    (
      fromLogin
    , buildAxisHost
    , buildHost
    , buildURL
    , addParams
    ) where

import           Data.ByteString.Char8  as BSC
import           Network.URL
import           Network.Vapix.Types
import           Network.Wreq

fromLogin :: AxisLogin -> Maybe Auth
fromLogin (AxisLogin user pass _ _) = basicAuth (BSC.pack user) (BSC.pack pass)

buildAxisHost :: AxisLogin -> Host
buildAxisHost login = buildHost (axisHost login) (Just . fromIntegral . axisPort $ login)

buildHost :: String -> Maybe Integer -> Host
buildHost h p = Host (HTTP False) h p

buildURL :: Host -> String -> URL
buildURL urlHost urlPath = URL (Absolute urlHost) urlPath []

addParams :: URL -> [(String, String)] -> URL
addParams url urlParams = Prelude.foldl (\u p -> add_param u p) url urlParams