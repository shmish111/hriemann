module Network.Monitoring.Riemann.KClients where

import           Control.Concurrent
import           Control.Concurrent.Chan.Unagi          as Unagi
import           Control.Concurrent.Chan.Unagi.Bounded  as BUnagi
import           Control.Concurrent.KazuraQueue
import           Data.Foldable                          ( toList )
import           Data.Sequence                          as Seq
import qualified Network.Monitoring.Riemann.Proto.Event as PE
import           Network.Monitoring.Riemann.TCP         as TCP
import           Network.Socket

data KClient = KClient (Unagi.InChan (Command PE.Event))

data Command a = Message a
               | Stop (MVar ())

class Source s where
    takeItem :: s a -> IO a
    tryTakeItem :: s a -> IO (Maybe a)

instance Source Unagi.OutChan where
    takeItem = Unagi.readChan
    tryTakeItem s = do
        item <- Unagi.readChan s
        return $ Just item

instance Source MVar where
    takeItem = takeMVar
    tryTakeItem = tryTakeMVar

class Sink s where
    putItem :: s a -> a -> IO ()
    tryPutItem :: s a -> a -> IO Bool

instance Sink BUnagi.InChan where
    putItem = BUnagi.writeChan
    tryPutItem = tryWriteChan

riemannConsumer :: Source s
                => TCPConnection
                -> s (Command (Seq PE.Event))
                -> IO ()
riemannConsumer connection input =
    loop
  where
    loop = do
        cmd <- takeItem input
        case cmd of
            Message events -> do
                TCP.sendEvents connection $ toList events
                loop
            Stop s -> do
                putStrLn "stopping riemann consumer"
                putMVar s ()

batchConsumer :: (Source src, Sink snk) => Int -> src a -> snk b -> IO ()
batchConsumer batchSize source sink = do
    msg <- drainQueue batchSize source
    putItem sink msg


drainQueue :: Source s => Int -> s a -> IO (Seq a)
drainQueue n input = do
    msg <- tryTakeItem input
    case msg of
        Just a -> loop $ singleton a
        Nothing -> return empty
  where
    loop diffAs = if Seq.length diffAs == n
                  then return diffAs
                  else do
                      msg <- tryTakeItem input
                      case msg of
                          Nothing -> return diffAs
                          Just a -> loop $ diffAs |> a

overflowConsumer :: (Source src, Sink snk)
                 => src a
                 -> snk a
                 -> (a -> IO ())
                 -> IO ()
overflowConsumer source sink overflow =
    loop
  where
    loop = do
        cmd <- takeItem source
        wrote <- tryPutItem sink cmd
        if wrote
            then loop
            else do
                overflow cmd
                loop

newClient :: HostName -> Port -> Int -> Int -> (PE.Event -> IO ()) -> IO KClient
newClient hostname port batchSize overflowSize overflow
    | batchSize <= 0 = error "Batch Size must be positive"
    | otherwise = do
          connection <- TCP.tcpConnection hostname port
          (inChan, outChan) <- Unagi.newChan
          (rinChan, routChan) <- BUnagi.newChan overflowSize
          forkIO $ overflowConsumer outChan rinChan overflow
          forkIO $ riemannConsumer batchSize riemannChan connection
          return $ KClient inChan