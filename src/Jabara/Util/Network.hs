module Jabara.Util.Network (
  parsePortNumber
) where

import Network.Socket (PortNumber(..))
import Text.Read (readMaybe)

parsePortNumber :: String -> Either String PortNumber
parsePortNumber s = case readMaybe s of
                      Nothing -> Left ("invalid port number -> " ++ s)
                      Just i  -> Right $ fromInteger i

