{-# LANGUAGE MultiParamTypeClasses #-}
module Network.Monitoring.Riemann.TCPClient where

import qualified Data.Sequence as Seq
import Network.Monitoring.Riemann.Client (Client, close, sendEvent)
import qualified Network.Monitoring.Riemann.TCP as TCP
import Network.Monitoring.Riemann.TCP (Port, TCPConnection)
import Network.Socket (HostName)

newtype TCPClient =
  TCPClient TCPConnection

{-|
    A new TCPClient

    The TCPClient is a 'Client' that will send single events synchronously over TCP.

    Current time and host name will be set if not provided.

    '''Note''': We never use IPv6 address resolved for given hostname.
-}
tcpClient :: HostName -> Port -> IO TCPClient
tcpClient h p = do
  c <- TCP.tcpConnection h p
  pure $ TCPClient c

instance Client IO TCPClient where
  sendEvent (TCPClient connection) event =
    TCP.sendEvents connection $ Seq.singleton event
  close _ = print "close"
