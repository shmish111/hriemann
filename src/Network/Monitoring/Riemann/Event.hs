{-|
Module      : Network.Monitoring.Riemann.Event
Description : A module for building Riemann events

Build an event which can then be sent to Riemann using a 'Network.Monitoring.Riemann.Client.Client'

Events are built by composing helper functions that set Riemann fields and applying to one of the Event constructors:

@
  Event.ok "my service"
& Event.description "my description"
& Event.metric (length [ "some data" ])
& Event.ttl 20
& Event.tags ["tag1", "tag2"]
@

With this design you are encouraged to create an event with one of 'Event.ok', 'Event.warn' or 'Event.failure'.

This has been done because we found that it is best to avoid services like @my.service.success@ and @my.service.error@ (that's what the Riemann state field is for).

You can use your own states using @Event.info & Event.state "trace"@ however this is discouraged as it doesn't show up nicely in riemann-dash.
-}
module Network.Monitoring.Riemann.Event where

import           Control.Concurrent
import qualified Data.ByteString.Lazy.Char8                 as BC
import           Data.Maybe
import           Data.Sequence
import           Data.Time.Clock.POSIX
import           Network.HostName
import qualified Network.Monitoring.Riemann.Proto.Attribute as Attribute
import qualified Network.Monitoring.Riemann.Proto.Event     as Event
import qualified Network.Monitoring.Riemann.Proto.Msg       as Msg
import           Text.ProtocolBuffers.Basic                 as Basic
import qualified Text.ProtocolBuffers.Header                as P'

type Service = String

type State = String

type Event = Event.Event

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
    Add local hostname and current time to an Event

    This will not override any host or time in the provided event
-}
withDefaults :: Seq Event.Event -> IO (Seq Event.Event)
withDefaults e = do
    now <- fmap round getPOSIXTime
    hostname <- getHostName
    return $ fmap (addTimeAndHost now hostname) e
  where
    addTimeAndHost now hostname e =
        let now' = if isJust $ Event.time e then Event.time e else Just now
            host = if isJust $ Event.host e
                   then Event.host e
                   else toField hostname
        in
            e { Event.time = now', Event.host = host }
