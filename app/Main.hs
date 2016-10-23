module Main where

import           Control.Concurrent
import           Control.Monad                            (forM_)
import           Data.Function
import           Network.Monitoring.Riemann.BatchClient
import           Network.Monitoring.Riemann.Client
import qualified Network.Monitoring.Riemann.Event         as Event
import           Network.Monitoring.Riemann.LoggingClient
import           Network.Monitoring.Riemann.TCPClient

main :: IO ()
main = do
--     client <- batchClient "localhost" 5555 100 100 print
    client <- bc "localhost" 5555 100 print
    putStrLn "doing some IO work"
    event <- pure $
                 Event.warn "my service"
                     & Event.description "my description"
                     & Event.metric (length [ "some data" ])
                     & Event.ttl 20
                     & Event.tags [ "tag1", "tag2" ]
    forM_ [1 .. 1000000] $ \i -> sendEvent client $ event & Event.metric (i :: Int)
    close client
    putStrLn "finished"
