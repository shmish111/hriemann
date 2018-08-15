{-# LANGUAGE LambdaCase #-}

module Network.Monitoring.Riemann.BatchClient where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import qualified Control.Concurrent.Chan.Unagi as Unagi
import Control.Concurrent.KazuraQueue
  ( Queue
  , lengthQueue
  , newQueue
  , readQueue
  , tryReadQueue
  , writeQueue
  )
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), singleton)
import Network.Monitoring.Riemann.Client (Client, close, sendEvent)
import qualified Network.Monitoring.Riemann.Proto.Event as PE
import qualified Network.Monitoring.Riemann.TCP as TCP
import Network.Socket (HostName)
import System.IO (hPutStrLn, stderr)

newtype BatchClient =
  BatchClient (Unagi.InChan LogCommand)

newtype BatchClientNoBuffer =
  BatchClientNoBuffer (Queue LogCommand)

data LogCommand
  = Event PE.Event
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
batchClient ::
     HostName -> TCP.Port -> Int -> Int -> (PE.Event -> IO ()) -> IO BatchClient
batchClient hostname port bufferSize batchSize overflow
  | batchSize <= 0 = error "Batch Size must be positive"
  | otherwise = do
    connection <- TCP.tcpConnection hostname port
    (inChan, outChan) <- Unagi.newChan
    q <- newQueue
    _ <- forkIO $ overflowConsumer outChan q bufferSize overflow
    _ <- forkIO $ riemannConsumer batchSize q connection
    pure $ BatchClient inChan

bufferlessBatchClient :: HostName -> TCP.Port -> Int -> IO BatchClientNoBuffer
bufferlessBatchClient hostname port batchSize
  | batchSize <= 0 = error "Batch Size must be positive"
  | otherwise = do
    connection <- TCP.tcpConnection hostname port
    q <- newQueue
    _ <- forkIO $ riemannConsumer batchSize q connection
    pure $ BatchClientNoBuffer q

overflowConsumer ::
     Unagi.OutChan LogCommand
  -> Queue LogCommand
  -> Int
  -> (PE.Event -> IO ())
  -> IO ()
overflowConsumer outChan q bufferSize f = loop
  where
    loop = do
      cmd <- Unagi.readChan outChan
      case cmd of
        Event event -> do
          qSize <- lengthQueue q
          if qSize >= bufferSize
            then do
              f event
              loop
            else do
              writeQueue q cmd
              loop
        Stop _ -> do
          hPutStrLn stderr "stopping log consumer"
          writeQueue q cmd

drainAll :: Queue a -> Int -> IO (Seq a)
drainAll q n = do
  msg <- readQueue q
  loop $ singleton msg
  where
    loop msgs =
      if Seq.length msgs >= n
        then pure msgs
        else do
          msg <- tryReadQueue q
          case msg of
            Just m -> loop $ msgs |> m
            Nothing -> pure msgs

riemannConsumer :: Int -> Queue LogCommand -> TCP.TCPConnection -> IO ()
riemannConsumer batchSize q connection = loop
  where
    loop = do
      cmds <- drainAll q batchSize
      let (events, stops) =
            separate
              (\case
                 Event e -> Left e
                 Stop s -> Right s)
              cmds
      TCP.sendEvents connection events
      if Seq.null stops
        then loop
        else let s = Seq.index stops 0
              in do hPutStrLn stderr "stopping riemann consumer"
                    putMVar s ()

separate :: (a -> Either b c) -> Seq a -> (Seq b, Seq c)
separate f = foldl g (Seq.empty, Seq.empty)
  where
    g (bs, cs) a =
      case f a of
        Left b -> (bs |> b, cs)
        Right c -> (bs, cs |> c)

instance Client BatchClient where
  sendEvent (BatchClient inChan) event = Unagi.writeChan inChan $ Event event
  close (BatchClient inChan) = do
    s <- newEmptyMVar
    Unagi.writeChan inChan (Stop s)
    takeMVar s

instance Client BatchClientNoBuffer where
  sendEvent (BatchClientNoBuffer q) event = writeQueue q $ Event event
  close (BatchClientNoBuffer q) = do
    s <- newEmptyMVar
    writeQueue q (Stop s)
    takeMVar s
