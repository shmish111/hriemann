module Network.Monitoring.Riemann.BatchClient where

import           Control.Concurrent
import           Control.Concurrent.Chan.Unagi          as Unagi
import           Control.Concurrent.Chan.Unagi.Bounded  as BUnagi
import           Data.List
import           Data.Maybe                             as Maybe
import           Network.Monitoring.Riemann.Client
import           Network.Monitoring.Riemann.Event       as Event
import qualified Network.Monitoring.Riemann.Proto.Event as PE
import           Network.Monitoring.Riemann.TCP         as TCP
import           Network.Socket

data BatchClient = BatchClient LogChan

type LogChan = (Unagi.InChan LogCommand, Unagi.OutChan LogCommand)

type RiemannChan = (BUnagi.InChan LogCommand, BUnagi.OutChan LogCommand)

data LogCommand = Event PE.Event
                | Stop (MVar ())

{-|
    A new BatchClient

    The BatchClient is a 'Client' that will do the following

    * Batch events into a specified batch size
    * If events are produced more quickly than Riemann can cope with, they will be passed to the overflow function

    Batching events is important for throughput, see <https://aphyr.com/posts/269-reaching-200k-events-sec>

    It is important to deal with back pressure, if the buffer of events to be sent to Riemann fills up, they will be
    passed to the overflow function until the buffer has space again. This overflow function can be as simple as 'print'

    Current time and host name will be set if not provided.

    '''Note''': We never use IPv6 address resolved for given hostname.
-}
batchClient :: HostName
            -> Port
            -> Int
            -> Int
            -> (PE.Event -> IO ())
            -> IO BatchClient
batchClient h p bufferSize batchSize overflow
    | batchSize <= 0 = error "Batch Size must be positive"
    | otherwise = do
          connection <- TCP.tcpConnection h p
          logChan <- Unagi.newChan
          riemannChan <- BUnagi.newChan bufferSize
          forkIO $ overflowConsumer logChan riemannChan overflow
          forkIO $ riemannConsumer batchSize riemannChan connection
          return $ BatchClient logChan

overflowConsumer :: LogChan -> RiemannChan -> (PE.Event -> IO ()) -> IO ()
overflowConsumer (_, outChan) (inChan, _) overflow =
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

takeFromChan :: Int
             -> BUnagi.OutChan LogCommand
             -> IO [(Element LogCommand, IO LogCommand)]
takeFromChan n outChan =
    mapM (\_ -> BUnagi.tryReadChan outChan) [1 .. n]

elementToMaybe :: (Element LogCommand, IO LogCommand)
               -> IO (Maybe LogCommand, IO LogCommand)
elementToMaybe (e, b) = do
    m <- BUnagi.tryRead e
    return (m, b)

riemannConsumer :: Int -> RiemannChan -> TCPConnection -> IO ()
riemannConsumer batchSize (_, outChan) connection =
    loop []
  where
    loop :: [(Maybe LogCommand, IO LogCommand)] -> IO ()
    loop [] = do
        items <- takeFromChan batchSize outChan
        new <- mapM elementToMaybe items
        loop new
    loop remaining = do
        items <- takeFromChan (batchSize - length remaining) outChan
        new <- mapM elementToMaybe items
        let (m, b) : xs = remaining ++ new
        x <- b -- block until first element is realized
        let (realizedCmds, nothings) =
                partition (\(m, b) -> Maybe.isJust m) ((Just x, b) : xs)
            cmds = map (\(Just cmd, _) -> cmd) realizedCmds
            (events', stops') = partition (\cmd -> case cmd of
                                               Event event -> True
                                               Stop s      -> False)
                                          cmds
            events = fmap (\(Event e) -> e) events'
            stops = fmap (\(Stop s) -> s) stops'
        TCP.sendEvents connection events
        if null stops
            then loop nothings
            else let s : _ = stops
                 in do
                     putStrLn "stopping riemann consumer"
                     putMVar s ()

closeBatchClient :: BatchClient -> IO ()
closeBatchClient (BatchClient (inChan, _)) = do
    s <- newEmptyMVar
    Unagi.writeChan inChan (Stop s)
    takeMVar s

instance Client BatchClient where
    sendEvent (BatchClient (inChan, _)) event =
        Unagi.writeChan inChan $ Event event
