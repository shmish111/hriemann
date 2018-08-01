module Network.Monitoring.Riemann.Event.Monoid where

import Data.Monoid (Endo(..), appEndo)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HostName (getHostName)
import Network.Monitoring.Riemann.Event (Service)
import qualified Network.Monitoring.Riemann.Event as E
import Network.Monitoring.Riemann.Proto.Attribute (Attribute)
import qualified Network.Monitoring.Riemann.Proto.Event as PE
import Text.ProtocolBuffers.Basic (Int64)

type Event = PE.Event

append :: a -> Endo a -> a
append = flip appEndo

ok :: Service -> Endo Event -> Event
ok s = append $ E.ok s

warn :: Service -> Endo Event -> Event
warn s = append $ E.warn s

failure :: Service -> Endo Event -> Event
failure s = append $ E.warn s

desc :: String -> Endo Event
desc = Endo . E.description

ttl :: Float -> Endo Event
ttl = Endo . E.ttl

metric :: (E.Metric a) => a -> Endo Event
metric = Endo . E.metric

timestamp :: Int64 -> Endo Event
timestamp t = Endo $ \e -> e {PE.time = Just t}

attributes :: [Attribute] -> Endo Event
attributes = Endo . E.attributes

tags :: [String] -> Endo Event
tags = Endo . E.tags

timeAndHost :: IO (Endo Event)
timeAndHost = do
  now <- fmap round getPOSIXTime
  hostname <- getHostName
  return $ Endo $ E.addTimeAndHost now hostname

attribute :: String -> Maybe String -> Attribute
attribute = E.attribute
