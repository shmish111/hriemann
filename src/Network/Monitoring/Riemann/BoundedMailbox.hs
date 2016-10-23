module Network.Monitoring.Riemann.BoundedMailbox where

import           Control.Applicative            ( (<$>), (<*)
                                                , Alternative(empty, (<|>))
                                                , Applicative(pure, (*>), (<*>)) )
import qualified Control.Concurrent.STM         as S
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Monad                  ( void )
import           Pipes.Concurrent

spawnBounded :: Int -> IO (Output a, Input a, STM Bool)
spawnBounded n = do
    q <- S.newTBQueueIO n
    (write, read, full) <- return ( TBQ.writeTBQueue q
                                  , TBQ.readTBQueue q
                                  , TBQ.isFullTBQueue q
                                  )
    sealed <- S.newTVarIO False
    let seal = S.writeTVar sealed True

    {- Use weak TVars to keep track of whether the 'Input' or 'Output' has been
       garbage collected.  Seal the mailbox when either of them becomes garbage
       collected.
    -}
    rSend <- newTVarIO ()
    void $ mkWeakTVar rSend (S.atomically seal)
    rRecv <- newTVarIO ()
    void $ mkWeakTVar rRecv (S.atomically seal)

    let sendOrEnd a = do
            b <- S.readTVar sealed
            if b
                then return False
                else do
                    write a
                    return True
        readOrEnd = (Just <$> read) <|>
            (do
                 b <- S.readTVar sealed
                 S.check b
                 return Nothing)
        _send a = sendOrEnd a <* readTVar rSend
        _recv = readOrEnd <* readTVar rRecv
    return (Output _send, Input _recv, full)