{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Monitoring.Riemann.LoggingClient where

import Network.Monitoring.Riemann.Client (Client, close, sendEvent)

data LoggingClient =
  LoggingClient

{-|
    A new LoggingClient

    The LoggingClient is a 'Client' that will simply print events
-}
loggingClient :: LoggingClient
loggingClient = LoggingClient

instance Client IO LoggingClient where
  sendEvent _ = print
  close _ = print "close"
