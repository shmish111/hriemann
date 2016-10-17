module Main where

import           Data.Function
import qualified Network.Monitoring.Riemann.Event as Event
import           Network.Monitoring.Riemann.TCP

main :: IO ()
main = do
    c <- tcpConnect "localhost" 5555
    putStrLn "doing some IO work"
    event <- pure $
                 Event.ok "my service"
                     & Event.description "my description"
                     & Event.metric (length [ "some data" ])
                     & Event.ttl 20
                     & Event.tags [ "tag1", "tag2" ]
    Event.sendEvent c event
    putStrLn "do somethign else"
    event <- pure $ Event.ok "my other service"
    Event.sendEvent c event
    putStrLn "finished"
