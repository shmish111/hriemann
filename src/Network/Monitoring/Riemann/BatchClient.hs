module Network.Monitoring.Riemann.BatchClient where

import           Control.Concurrent
import           Control.Concurrent.Chan.Unagi          as Unagi
import           Control.Concurrent.Chan.Unagi.Bounded  as BUnagi
import           Network.Monitoring.Riemann.Client
import           Network.Monitoring.Riemann.Event       as Event
import qualified Network.Monitoring.Riemann.Proto.Event as PE
import           Network.Monitoring.Riemann.TCP         as TCP
import           Network.Socket

data BatchClient = BatchClient TCPConnection LogChan BufferChan RiemannChan

type LogChan = (Unagi.InChan LogCommand, Unagi.OutChan LogCommand)

type BufferChan = (BUnagi.InChan LogCommand, BUnagi.OutChan LogCommand)

type RiemannChan = (BUnagi.InChan LogCommand, BUnagi.OutChan LogCommand)

data LogCommand = Event PE.Event
                | Stop (MVar ())

{-|
    A new BatchClient

    The BatchClient is a 'Client' that will do the following

    * Batch events into a specified batch size - not implemented yet!!!
    * If events are produced more quickly than Riemann can cope with, they will be passed to the overflow function

    Batching events is important for throughput, see <https://aphyr.com/posts/269-reaching-200k-events-sec>

    It is important to deal with back pressure, if the buffer of events to be sent to Riemann fills up, they will be
    passed to the overflow function until the buffer has space again. This overflow function can be as simple as 'print'

    '''Note''': We never use IPv6 address resolved for given hostname.
-}
batchClient :: HostName -> Port -> Int -> (PE.Event -> IO ()) -> IO BatchClient
batchClient h p bufferSize overflow = do
    connection <- TCP.tcpConnection h p
    logChan <- Unagi.newChan
    bufferChan <- BUnagi.newChan bufferSize
    riemannChan <- BUnagi.newChan 1
    forkIO $ consumer logChan bufferChan overflow
    forkIO $ bufferConsumer bufferChan riemannChan
    forkIO $ riemannConsumer riemannChan connection
    return $ BatchClient connection logChan bufferChan riemannChan

consumer :: LogChan -> BufferChan -> (PE.Event -> IO ()) -> IO ()
consumer (_, outChan) (inChan, _) overflow =
    loop
  where
    loop = do
        cmd <- Unagi.readChan outChan
        case cmd of
            Event event -> do
                wrote <- tryWriteChan inChan cmd
                if wrote
                    then loop
                    else do
                        overflow event
                        loop
            Stop s -> do
                putStrLn "stopping log consumer"
                BUnagi.writeChan inChan (Stop s)

bufferConsumer :: BufferChan -> RiemannChan -> IO ()
bufferConsumer (_, outChan) (inChan, _) =
    loop
  where
    loop = do
        cmd <- BUnagi.readChan outChan
        case cmd of
            Event event -> do
                BUnagi.writeChan inChan cmd
                loop
            Stop s -> do
                putStrLn "stopping buffer consumer"
                BUnagi.writeChan inChan (Stop s)

riemannConsumer :: RiemannChan -> TCPConnection -> IO ()
riemannConsumer (inChan, outChan) connection =
    loop
  where
    loop = do
        cmd <- BUnagi.readChan outChan
        case cmd of
            Event event -> do
                events <- Event.withDefaults [ event ]
                TCP.sendEvents connection events
                loop
            Stop s -> do
                putStrLn "stopping riemann consumer"
                putMVar s ()

closeBatchClient :: BatchClient -> IO ()
closeBatchClient (BatchClient _ (inChan, _) _ _) = do
    s <- newEmptyMVar
    Unagi.writeChan inChan (Stop s)
    takeMVar s

instance Client BatchClient where
    sendEvents c = mapM_ (sendEvent c)
    sendEvent (BatchClient connection (inChan, _) _ _) event =
        Unagi.writeChan inChan $ Event event