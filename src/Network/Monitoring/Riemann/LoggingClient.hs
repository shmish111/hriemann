module Network.Monitoring.Riemann.LoggingClient where

import           Control.Concurrent
import           Network.Monitoring.Riemann.Client

data LoggingClient = LoggingClient

loggingClient = LoggingClient

instance Client LoggingClient where
    sendEvents client = mapM_ print
    sendEvent client = print
