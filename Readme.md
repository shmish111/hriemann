# A Riemann Client for Haskell

[![Build Status](https://travis-ci.org/shmish111/hriemann.svg?branch=master)](https://travis-ci.org/shmish111/hriemann)

This is a Riemann client for Haskell with an API based on my experience using Riemann in a production environment.

It was initially inspired by [riemann-hs](https://github.com/tel/riemann-hs) however I felt things could be a little simpler.

* No Lenses
* Doesn't use a monad transformer
* Currently this library is only for sending events, not querying
* No UDP client
* Async TCP sending
* Batching and async error handling
* Deal with back pressure

Please be aware that this is currently a work in progress and hasn't been well tested yet. I still have the following features to add:

* Tests!!!
* Timeout if events take too long to send

## Use pipes?

Originally I wanted to avoid pipes as it's just another dependency and Unagi seems to be quite a bit faster, however after implementing most of the required features I can see that it would be really nice if the 'Clients' were made of individual components that compose. You would have 1 producer which is connected to `sendEvent`, consumers such as a TCP consumer and a stdout consumer as well as pipes such as batch pipe and overflow pipe.

## Usage

```haskell
module Main where

import           Data.Function
import qualified Network.Monitoring.Riemann.Event as Event
import           Network.Monitoring.Riemann.TCP
import qualified Data.Sequence as Seq

main :: IO ()
main = do
    c <- tcpConnection "localhost" 5555
    putStrLn "doing some IO work"
    let event1 = Event.ok "my service"
                   & Event.description "my description"
                   & Event.metric (length [ "some data" ])
                   & Event.ttl 20
                   & Event.tags [ "tag1", "tag2" ]
    Event.sendEvents c (Seq.singleton event1)
    putStrLn "do something else"
    let event2 = Event.ok "my other service"
    Event.sendEvents c (Seq.singleton event2)
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

Alternatively there is a Monoid based api for creating events:

```haskell
import Data.Monoid (Endo, (<>))
import Network.Monitoring.Riemann.Event.Monoid


eventA :: Endo Event
eventA = ttl 10 <> metric (1 :: Int) <> attributes [attribute "something" Nothing]

eventB :: Endo Event
eventB = tags ["tag 1"]

compositeEvent :: Event
compositeEvent = ok "some service" (eventA <> eventB)
```
