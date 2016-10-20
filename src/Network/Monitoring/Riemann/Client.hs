module Network.Monitoring.Riemann.Client where

import qualified Network.Monitoring.Riemann.Proto.Event as Event

{-|
    A Client is able to send 'Network.Monitoring.Riemann.Proto.Event.Event's to Riemann
-}
class Client a where
    sendEvent :: a -> Event.Event -> IO ()
