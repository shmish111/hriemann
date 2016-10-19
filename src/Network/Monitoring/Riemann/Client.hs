module Network.Monitoring.Riemann.Client where

import qualified Network.Monitoring.Riemann.Proto.Event as Event
import qualified Network.Monitoring.Riemann.Proto.Msg   as Msg

{-|
    A Client is able to send 'Network.Monitoring.Riemann.Proto.Msg.Msg's and 'Network.Monitoring.Riemann.Proto.Event.Event's to Riemann
-}
class Client a where
    sendEvents :: a -> [Event.Event] -> IO ()
    sendEvent :: a -> Event.Event -> IO ()
