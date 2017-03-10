{-|
Module      : Network.Monitoring.Riemann.Event
Description : A module for building Riemann events

Build an event which can then be sent to Riemann using a 'Network.Monitoring.Riemann.Client.Client'

Events are built by composing helper functions that set Riemann fields and applying to one of the Event constructors:

@
  E.ok "my service"
& E.description "my description"
& E.metric (length [ "some data" ])
& E.ttl 20
& E.tags ["tag1", "tag2"]
@

With this design you are encouraged to create an event with one of 'E.ok', 'E.warn' or 'E.failure'.

This has been done because we found that it is best to avoid services like @my.service.success@ and @my.service.error@ (that's what the Riemann state field is for).

You can use your own states using @E.info & E.state "trace"@ however this is discouraged as it doesn't show up nicely in riemann-dash.
-}
module Network.Monitoring.Riemann.Event where

import qualified Data.ByteString.Lazy.Char8                 as BC
import           Data.Maybe
import           Data.Monoid                                ((<>))
import           Data.Sequence
import           Data.Time.Clock.POSIX
import           Network.HostName
import qualified Network.Monitoring.Riemann.Proto.Attribute as Attribute
import           Network.Monitoring.Riemann.Proto.Event     (Event)
import qualified Network.Monitoring.Riemann.Proto.Event     as E
import           Text.ProtocolBuffers.Basic                 as Basic
import qualified Text.ProtocolBuffers.Header                as P'

type Service = String

type State = String

emptyEvent :: Event
emptyEvent = P'.defaultValue

toField :: String -> Maybe Basic.Utf8
toField string = Just $ Basic.Utf8 $ BC.pack string

info :: Service -> Event
info service = P'.defaultValue { E.service = toField service }

state :: State -> Event -> Event
state s e = e { E.state = toField s }

ok :: Service -> Event
ok service = state "ok" $ info service

warn :: Service -> Event
warn service = state "warn" $ info service

failure :: Service -> Event
failure service = state "failure" $ info service

description :: String -> Event -> Event
description d e = e { E.description = toField d }

class Metric a where
    setMetric :: a -> Event -> Event

instance Metric Int where
    setMetric m e = e { E.metric_sint64 = Just $ fromIntegral m }

instance Metric Integer where
    setMetric m e = e { E.metric_sint64 = Just $ fromIntegral m }

instance Metric Int64 where
    setMetric m e = e { E.metric_sint64 = Just m }

instance Metric Double where
    setMetric m e = e { E.metric_d = Just m }

instance Metric Float where
    setMetric m e = e { E.metric_f = Just m }

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
metric :: (Metric a) => a -> Event -> Event
metric = setMetric

ttl :: Float -> Event -> Event
ttl t e = e { E.ttl = Just t }

tags :: [String] -> Event -> Event
tags ts e = let tags' = fromList $ fmap (Basic.Utf8 . BC.pack) ts
            in
                e { E.tags = tags' <> E.tags e }

attributes :: [Attribute.Attribute] -> Event -> Event
attributes as e = e { E.attributes = fromList as <> E.attributes e }

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
withDefaults :: Seq Event -> IO (Seq Event)
withDefaults e = do
    now <- fmap round getPOSIXTime
    hostname <- getHostName
    return $ fmap (addTimeAndHost now hostname) e

addTimeAndHost :: Int64 -> String -> Event -> Event
addTimeAndHost now hostname e
    | isJust (E.time e) && isJust (E.host e) =
          e
    | isJust (E.time e) = e { E.host = toField hostname }
    | isJust (E.host e) = e { E.time = Just now }
    | otherwise = e { E.time = Just now, E.host = toField hostname }
