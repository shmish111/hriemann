module Main where

import           Control.Concurrent
import           Data.Function
import qualified Network.Monitoring.Riemann.Event as Event
import Network.Monitoring.Riemann.TCPClient
import Network.Monitoring.Riemann.LoggingClient
import Network.Monitoring.Riemann.BatchClient
import Network.Monitoring.Riemann.Client

main :: IO ()
main = do
--     client <- tcpClient "localhost" 5555
--     let client = loggingClient
    client <- batchClient "localhost" 5555 2 1 print
    putStrLn "doing some IO work"
    event <- pure $
                 Event.warn "my service"
                     & Event.description "my description"
                     & Event.metric (length [ "some data" ])
                     & Event.ttl 20
                     & Event.tags [ "tag1", "tag2" ]
    sendEvent client $ event & Event.metric (1 :: Int)
--     threadDelay 5000000
--     putStrLn "do somethign else"
--     event <- pure $ Event.ok "my other service"
--     sendEvent client event
--     threadDelay 7000000
--     putStrLn "send again"
    sendEvent client $ event & Event.metric (2 :: Int)
    sendEvent client $ event & Event.metric (3 :: Int)
    sendEvent client $ event & Event.metric (4 :: Int)
    sendEvent client $ event & Event.metric (5 :: Int)
    sendEvent client $ event & Event.metric (6 :: Int)
    sendEvent client $ event & Event.metric (7 :: Int)
    threadDelay 1000000
    closeBatchClient client
    putStrLn "finished"