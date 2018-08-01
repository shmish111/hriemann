module Network.Monitoring.Riemann.TCPClient where

import Data.Sequence as Seq
import Network.Monitoring.Riemann.Client
import Network.Monitoring.Riemann.TCP as TCP
import Network.Socket

data TCPClient =
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
  return $ TCPClient c

instance Client TCPClient where
  sendEvent (TCPClient connection) event =
    TCP.sendEvents connection $ Seq.singleton event
  close _ = print "close"
