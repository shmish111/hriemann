module Network.Monitoring.Riemann.Event where

import           Control.Concurrent
import qualified Data.ByteString.Lazy.Char8                 as BC
import           Data.Sequence
import           Data.Time.Clock.POSIX
import           Network.HostName
import qualified Network.Monitoring.Riemann.Proto.Attribute as Attribute
import qualified Network.Monitoring.Riemann.Proto.Event     as Event
import qualified Network.Monitoring.Riemann.Proto.Msg       as Msg
import           Network.Monitoring.Riemann.TCP
import           Text.ProtocolBuffers.Basic                 as Basic
import qualified Text.ProtocolBuffers.Header                as P'

type Service = String

type State = String

toField :: String -> Maybe Basic.Utf8
toField string = Just $ Basic.Utf8 $ BC.pack string

info :: Service -> Event.Event
info service = P'.defaultValue { Event.service = toField service }

state :: State -> Event.Event -> Event.Event
state s e = e { Event.state = toField s }

ok :: Service -> Event.Event
ok service = state "ok" $ info service

warn :: Service -> Event.Event
warn service = state "warn" $ info service

failure :: Service -> Event.Event
failure service = state "failure" $ info service

description :: String -> Event.Event -> Event.Event
description d e = e { Event.description = toField d }

class Metric a where
    setMetric :: a -> Event.Event -> Event.Event

instance Metric Int where
    setMetric m e = e { Event.metric_sint64 = Just $ fromIntegral m }

instance Metric Integer where
    setMetric m e = e { Event.metric_sint64 = Just $ fromIntegral m }

instance Metric Int64 where
    setMetric m e = e { Event.metric_sint64 = Just m }

instance Metric Double where
    setMetric m e = e { Event.metric_d = Just m }

instance Metric Float where
    setMetric m e = e { Event.metric_f = Just m }

{-|
    Note that since Riemann's protocol has separate types for integers, floats and doubles, you need to specify which
    type you are using. For example, this won't work:

    @
    metric 1 myEvent
    @

    Instead use:

    @
    metric (1 :: Int) myEvent
    @
-}
metric :: (Metric a) => a -> Event.Event -> Event.Event
metric = setMetric

ttl :: Float -> Event.Event -> Event.Event
ttl t e = e { Event.ttl = Just t }

tags :: [String] -> Event.Event -> Event.Event
tags ts e = let tags' = fromList $ fmap (Basic.Utf8 . BC.pack) ts
            in
                e { Event.tags = tags' }

attributes :: [Attribute.Attribute] -> Event.Event -> Event.Event
attributes as e = e { Event.attributes = fromList as }

attribute :: String -> Maybe String -> Attribute.Attribute
attribute k mv = let k' = (Basic.Utf8 . BC.pack) k
                     mv' = fmap (Basic.Utf8 . BC.pack) mv
                 in
                     P'.defaultValue { Attribute.key = k'
                                     , Attribute.value = mv'
                                     }

{-|
    Send a list of Riemann events - only use this function if you want to specify time and hostname
-}
sendEvents' :: TCPState -> [Event.Event] -> IO ()
sendEvents' client events =
    sendMsg client $ P'.defaultValue { Msg.events = fromList events }

withDefaults :: [Event.Event] -> IO [Event.Event]
withDefaults e = do
    now <- fmap round getPOSIXTime
    hostname <- getHostName
    return $ fmap (addTimeAndHost now hostname) e
  where
    addTimeAndHost now hostname e =
        e { Event.time = Just now, Event.host = toField hostname }

{-|
    Send an event to Riemann - will set current time and hostname
-}
sendEvent :: TCPState -> Event.Event -> IO ThreadId
sendEvent client event =
    forkIO $ do
        events <- withDefaults [ event ]
        sendEvents' client events

{-|
    Send a list of events to Riemann - will set current time and hostname on each event
-}
sendEvents :: TCPState -> [Event.Event] -> IO ThreadId
sendEvents client events =
    forkIO $ do
        events' <- withDefaults events
        sendEvents' client events
