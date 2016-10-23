module Network.Monitoring.Riemann.PipesStuff where

import           Control.Applicative                       ( (<|>) )
import           Control.Concurrent                        ( threadDelay )
import           Control.Monad
import           Data.Foldable                             ( forM_, toList )
import           Data.Function
import           Data.Monoid
import           Data.Sequence                             as Seq
import           Network.Monitoring.Riemann.BoundedMailbox
import qualified Network.Monitoring.Riemann.Event          as Event
import qualified Network.Monitoring.Riemann.Proto.Event    as PE
import           Network.Monitoring.Riemann.TCP            as TCP
import           Pipes
import           Pipes.Concurrent
import qualified Pipes.Prelude                             as P

{-|
    Takes a batch size and an 'Input' and returns a 'Producer' that will produce batches of up-to the batch size when
    they are pulled from downstream.

-}
batchProducer :: Int -> Input a -> Producer (Seq a) IO ()
batchProducer n i = loop
  where
    loop = do
        lift $ print "here"
        ma <- lift $ atomically $ drainAll n i
        forM_ ma $ \a -> yield a
        loop

drainAll :: Int -> Input a -> STM (Maybe (Seq a))
drainAll n i = do
    ma <- recv i
    case ma of
        Nothing -> return Nothing
        Just a -> loop $ singleton a
  where
    loop diffAs = if Seq.length diffAs == n
                  then return (Just diffAs)
                  else do
                      ma <- recv i <|> return Nothing
                      case ma of
                          Nothing -> return (Just diffAs)
                          Just a -> loop $ diffAs |> a

riemannConsumer :: TCP.TCPConnection -> Consumer (Seq PE.Event) IO ()
riemannConsumer connection =
    loop
  where
    loop = do
        events <- await
        lift $ TCP.sendEvents connection $ toList events
        loop

overflowConsumer :: Output PE.Event -> STM Bool -> Consumer PE.Event IO ()
overflowConsumer o full =
    forever $ do
        events <- await
        lift $ do
            isFull <- atomically full
            if isFull
                then print "full"
                else do
                    print "empty"
                    res <- atomically $ send o events
                    unless res $ print "fail"

oc :: Output PE.Event -> STM Bool -> PE.Event -> IO ()
oc o full event = do
    isFull <- atomically full
    if isFull
        then print "full"
        else do
            print "empty"
            res <- atomically $ send o event
            if res then
                print "success"
            else
                print "fail"

overflowProducer :: Pipe PE.Event PE.Event IO ()
overflowProducer = do
    (output, input, full) <- lift $ spawnBounded 4
    forever $ do
        lift $ print "waiting"
        event <- await
        lift $ oc output full event
        lift $ print "here"
        fromInput input

printEvents :: Consumer PE.Event IO ()
printEvents = forever $ do
    lift $ print "waiting"
    events <- await
    lift $ print $ "got " ++ show events

-- overflowPipe :: Pipe PE.Event PE.Event IO ()
-- overflowPipe = do
--     (output, input, full) <- lift $ spawnBounded 4
--     forkIO $ runEffect $ overflowConsumer output full
--     let overProd = overflowProducer input
--     overProd
-- wrap :: Monad m => Producer a m r -> Producer (Maybe a) m r
-- wrap p = do
--     p >-> P.map Just
--     forever $ yield Nothing
--
-- myPipe :: Monad m => Pipe a (a, a) m r
-- myPipe = forever $ do
--     x <- await
--     y <- await
--     yield (x, y)
--
-- waiter :: Pipe a a IO r
-- waiter = forever $ do
--     x <- await
--     yield x
--     lift $ threadDelay 1000000
--
-- delay :: Double -> Pipe a a IO r
-- delay seconds = for cat $
--     \a -> do
--         yield a
--         lift $ threadDelay (truncate (seconds * 1000000))
testConsumer :: Consumer (Seq PE.Event) IO ()
testConsumer = loop
  where
    loop = do
        x <- await
        lift $ print $ "got " ++ show x
        lift $ threadDelay 1000000
        case Seq.length x of
            1 -> return ()
            _ -> loop

testProducer :: Producer PE.Event IO ()
testProducer = forever $ do
    lift $ threadDelay 1000000
    lift $ print "create"
    let event = Event.warn "my service"
            & Event.description "my description"
            & Event.metric (1 :: Int)
            & Event.ttl 20
            & Event.tags [ "tag1", "tag2" ]
    yield event

-- fpow n f x = iterate f x !! n
main :: IO ()
main = do
    (output, input, full) <- spawnBounded 4
    let overCons = overflowConsumer output full
    let drainProducer = batchProducer 1 input
    do
        connection <- TCP.tcpConnection "localhost" 5555
        event <- pure $
                     Event.warn "my service"
                         & Event.description "my description"
                         & Event.metric (1 :: Int)
                         & Event.ttl 20
                         & Event.tags [ "tag1", "tag2" ]
        --         x <- atomically $ do
        --                  send output event
        --                  send output event
        --                  send output event
        --         forkIO $
        --             forever $ do
        --                 threadDelay 1000000
        --                 y <- atomically $ send output event
        --                 return ()
        {-
    I need to have overCons have a forked loop for putting stuff in the mailbox and then inside the same function have a
    simple listener taking from the mailbox and yielding. Then that is a normal pipe.

    Same needs to be done for drainProducer but the other way round. Have a simple producer that await's for events and
    puts them in the mailbox then the drain mechanism takes them out and yields them
-}
--         forkIO $
--             runEffect $ testProducer >-> overCons
        runEffect $ testProducer >-> overflowProducer >-> printEvents
--         runEffect $ drainProducer >-> riemannConsumer connection