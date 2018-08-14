{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Monitoring.Riemann.LoggingClient where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.Monitoring.Riemann.Client (Client, close, sendEvent)

data LoggingClient =
  LoggingClient

{-|
    A new LoggingClient

    The LoggingClient is a 'Client' that will simply print events
-}
loggingClient :: LoggingClient
loggingClient = LoggingClient

instance MonadIO m => Client m LoggingClient where
  sendEvent _ = liftIO . print
  close _ = liftIO $ print "close"
