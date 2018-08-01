module Network.Monitoring.Riemann.LoggingClient where

import Network.Monitoring.Riemann.Client

data LoggingClient =
  LoggingClient

{-|
    A new LoggingClient

    The LoggingClient is a 'Client' that will simply print events
-}
loggingClient :: LoggingClient
loggingClient = LoggingClient

instance Client LoggingClient where
  sendEvent _ = print
  close _ = print "close"
