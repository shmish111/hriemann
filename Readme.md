# A Riemann Client for Haskell

This is a Riemann client for Haskell with an API based on my experience using Riemann in a production environment.

It was initially inspired by [riemann-hs](https://github.com/tel/riemann-hs) however I felt things could be a little simpler.

* No Lenses
* Doesn't use a monad transformer
* Currently this library is only for sending events, not querying
* No UDP client
* Async TCP sending
* Batching and async error handling (not implemented yet)

Please be aware that this is currently a work in progress and hasn't been well tested yet. I still have the following features to add:

* Optionally batch events before sending
* Timeout if events take too long to send
* Nicely log events to stdout if we fail to send to Riemann

## Usage

```haskell
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
```

Events are built by composing helper functions that set Riemann fields and applying to one of the Event constructors:

```haskell
  Event.ok "my service"
& Event.description "my description" 
& Event.metric (length [ "some data" ]) 
& Event.ttl 20 
& Event.tags ["tag1", "tag2"] 
```

With this design you are encouraged to create an event with one of `Event.ok`, `Event.warn` or `Event.failure`.

This has been done because we found that it is best to avoid services like `my.service.success` and `my.service.error` (that's what the Riemann state field is for).

You can use your own states using `Event.info & Event.state "trace"` however this is discouraged as it doesn't show up nicely in riemann-dash.
