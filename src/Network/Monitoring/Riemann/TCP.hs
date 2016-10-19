{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.Monitoring.Riemann.TCP
    ( tcpConnection
    , sendEvents
    , sendMsg
    , TCPConnection
    , Port
    ) where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception
import           Data.Binary.Put                        as Put
import qualified Data.ByteString.Lazy                   as BS
import qualified Data.ByteString.Lazy.Char8             as BC
import qualified Data.Maybe                             as Maybe
import qualified Data.Sequence                          as Seq
import           Data.Text                              (Text)
import qualified Network.Monitoring.Riemann.Event       as Event
import qualified Network.Monitoring.Riemann.Proto.Event as PE
import qualified Network.Monitoring.Riemann.Proto.Msg   as Msg
import           Network.Socket
import           Network.Socket.ByteString.Lazy         as NSB
import           Text.ProtocolBuffers.Get               as Get
import qualified Text.ProtocolBuffers.Header            as P'
import           Text.ProtocolBuffers.WireMessage       as WM

type ClientInfo = (HostName, Port, TCPStatus)

type TCPConnection = MVar ClientInfo

data TCPStatus = CnxClosed
               | CnxOpen (Socket, AddrInfo)
    deriving (Show)

type Port = Int

tcpConnection :: HostName -> Port -> IO TCPConnection
tcpConnection h p = do
    connection <- doConnect h p
    newMVar (h, p, CnxOpen connection)

getConnection :: ClientInfo -> IO (Socket, AddrInfo)
getConnection (h, p, CnxOpen (s, a)) =
    return (s, a)
getConnection (h, p, CnxClosed) =
    doConnect h p

tcpv4 :: AddrInfo -> Bool
tcpv4 addr = addrSocketType addr == Stream && addrFamily addr == AF_INET

doConnect :: HostName -> Port -> IO (Socket, AddrInfo)
doConnect hn po = do
    addrs <- getAddrInfo (Just $
                              defaultHints { addrFlags = [ AI_NUMERICSERV ] })
                         (Just hn)
                         (Just $ show po)
    case filter tcpv4 addrs of
        [] -> fail ("No accessible addresses in " ++ show addrs)
        (addy : _) -> do
            s <- socket AF_INET Stream defaultProtocol
            connect s (addrAddress addy)
            return (s, addy)

msgToByteString :: Msg.Msg -> BC.ByteString
msgToByteString msg = Put.runPut $ do
    Put.putWord32be $ fromIntegral $ WM.messageSize msg
    messagePutM msg

decodeMsg :: BC.ByteString -> Either String Msg.Msg
decodeMsg bs = let result = messageGet (BS.drop 4 bs)
               in
                   case result of
                       Left e -> Left e
                       Right (m, _) -> if Maybe.isNothing (Msg.ok m)
                                       then Left "error"
                                       else Right m

sendMsg :: TCPConnection -> Msg.Msg -> IO (Either Msg.Msg Msg.Msg)
sendMsg client msg = do
    clientInfo@(h, p, state) <- takeMVar client
    (s, a) <- getConnection clientInfo
    sendAll s $ msgToByteString msg
    bs <- NSB.recv s 4096
    result <- pure $ decodeMsg bs
    case result of
        Right m -> do
            putMVar client clientInfo
            return $ Right m
        Left e -> do
            putMVar client (h, p, CnxClosed)
            return $ Left msg

{-|
    Send a list of Riemann events
-}
sendEvents :: TCPConnection -> [PE.Event] -> IO ()
sendEvents client events = do
    result <- sendMsg client $
                  P'.defaultValue { Msg.events = Seq.fromList events }
    case result of
        Left msg -> print $ "failed to send" ++ show msg
        Right _  -> return ()
