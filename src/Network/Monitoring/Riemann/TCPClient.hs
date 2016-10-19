module Network.Monitoring.Riemann.TCPClient where

import           Control.Concurrent
import           Network.Monitoring.Riemann.Client
import           Network.Monitoring.Riemann.Event  as Event
import           Network.Monitoring.Riemann.TCP    as TCP
import           Network.Socket

data TCPClient = TCPClient TCPConnection

{-|
    A new TCPClient

    The TCPClient is a 'Client' that will send events asynchronously over TCP.

    Current time and host name will be set if not provided.

    '''Note''': We never use IPv6 address resolved for given hostname.
-}
tcpClient :: HostName -> Port -> IO TCPClient
tcpClient h p = do
    c <- TCP.tcpConnection h p
    return $ TCPClient c

instance Client TCPClient where
    sendEvents (TCPClient client) events = do
        forkIO $ do
            events <- Event.withDefaults events
            TCP.sendEvents client events
        return ()
    sendEvent (TCPClient client) event =
        TCP.sendEvents client [ event ]
