module Jabara.Util.Network (
  parsePortNumber
  , localHostAddress
) where

import Data.Either
import GHC.Base
import GHC.Num
import Network.Socket (withSocketsDo, getAddrInfo
                      , socket, Family(AF_INET), SocketType(Datagram), defaultProtocol
                      , connect, addrAddress, getSocketName, close
                      , PortNumber(..))
import Text.Read (readMaybe)

parsePortNumber :: String -> Either String PortNumber
parsePortNumber s = case readMaybe s of
                      Nothing -> Left ("invalid port number -> " ++ s)
                      Just i  -> Right $ fromInteger i

localHostAddress :: IO String
localHostAddress = withSocketsDo $ do
    (serveraddr:_) <- getAddrInfo Nothing (Just "128.0.0.0") (Just "echo")
    s <- socket AF_INET Datagram defaultProtocol
    connect s (addrAddress serveraddr)
    sockAddr <- getSocketName s
    let adrs = takeWhile (/=':') $ show sockAddr
    close s
    return adrs
