module Network.Monitoring.Riemann.LoggingClient where

import           Network.Monitoring.Riemann.Client

data LoggingClient = LoggingClient

{-|
    A new LoggingClient

    The LoggingClient is a 'Client' that will simply print events
-}
loggingClient = LoggingClient

instance Client LoggingClient where
    sendEvent client = print
    close client = print "close"
