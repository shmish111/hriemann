module Network.Monitoring.Riemann.LoggingClient where

import           Control.Concurrent
import           Network.Monitoring.Riemann.Client

data LoggingClient = LoggingClient

{-|
    A new LoggingClient

    The TCPClient is a 'Client' that will simply print events

    '''Note''': We never use IPv6 address resolved for given hostname.
-}
loggingClient = LoggingClient

instance Client LoggingClient where
    sendEvents client = mapM_ print
    sendEvent client = print
