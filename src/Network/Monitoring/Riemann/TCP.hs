{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.Monitoring.Riemann.TCP
  ( tcpConnection
  , sendEvents
  , sendMsg
  , TCPConnection
  , Port
  ) where

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import Control.Exception (try)
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Maybe as Maybe
import Data.Monoid ((<>))
import Data.Sequence (Seq)
import qualified Network.Monitoring.Riemann.Event as Event
import qualified Network.Monitoring.Riemann.Proto.Event as PE
import qualified Network.Monitoring.Riemann.Proto.Msg as Msg
import Network.Socket
  ( AddrInfo
  , AddrInfoFlag(AI_NUMERICSERV)
  , Family(AF_INET)
  , HostName
  , Socket
  , SocketType(Stream)
  , addrAddress
  , addrFamily
  , addrFlags
  , connect
  , defaultHints
  , defaultProtocol
  , getAddrInfo
  , socket
  )
import qualified Network.Socket.ByteString.Lazy as NSB
import System.IO (hPutStrLn, stderr)
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.ProtocolBuffers.WireMessage as WM

data ClientInfo = ClientInfo
  { _hostname :: HostName
  , _port :: Port
  , _status :: TCPStatus
  }

type TCPConnection = MVar ClientInfo

data TCPStatus
  = CnxClosed
  | CnxOpen (Socket, AddrInfo)
  deriving (Show)

type Port = Int

tcpConnection :: HostName -> Port -> IO TCPConnection
tcpConnection _hostname _port = do
  clientInfo <- doConnect $ ClientInfo {_status = CnxClosed, ..}
  newMVar clientInfo

doConnect :: ClientInfo -> IO ClientInfo
doConnect clientInfo@(_status -> CnxOpen _) = pure clientInfo
doConnect clientInfo = do
  hPutStrLn stderr $
    "(Re)connecting to Riemann: " <> _hostname clientInfo <> ":" <>
    show (_port clientInfo)
  addrs <-
    try $
    getAddrInfo
      (Just $ defaultHints {addrFlags = [AI_NUMERICSERV], addrFamily = AF_INET})
      (Just (_hostname clientInfo))
      (Just . show . _port $ clientInfo)
  let family = AF_INET
  case addrs of
    Right [] -> fail ("No accessible addresses in " ++ show addrs)
    Right (addy:_) -> do
      s <- socket family Stream defaultProtocol
      result <- try $ connect s (addrAddress addy)
      case result of
        Left err -> handleError err
        Right () -> pure $ clientInfo {_status = CnxOpen (s, addy)}
    Left err -> handleError err
  where
    handleError :: IOError -> IO ClientInfo
    handleError err = do
      hPutStrLn stderr $ "Connection to Riemann failed: " <> show err
      pure $ clientInfo {_status = CnxClosed}

msgToByteString :: Msg.Msg -> BC.ByteString
msgToByteString msg =
  Put.runPut $ do
    Put.putWord32be $ fromIntegral $ WM.messageSize msg
    WM.messagePutM msg

decodeMsg :: BC.ByteString -> Either String Msg.Msg
decodeMsg bs =
  let result = WM.messageGet (BS.drop 4 bs)
   in case result of
        Left e -> Left e
        Right (m, _) ->
          if Maybe.isNothing (Msg.ok m)
            then Left "error"
            else Right m

{-| Attempts to send a message and return the response.

If the connection is down, this function will trigger one reconnection attempt.
If that succeeds the message will be sent.
If it fails, the message is dropped and will need to be resent by you.
-}
sendMsg :: TCPConnection -> Msg.Msg -> IO (Either Msg.Msg Msg.Msg)
sendMsg client msg = go True
  where
    go reconnect = do
      clientInfo <- takeMVar client
      case (_status clientInfo, reconnect) of
        (CnxClosed, True) -> pure $ Left msg
        (CnxClosed, False) -> do
          newInfo <- doConnect clientInfo
          putMVar client newInfo
          go False
        (CnxOpen (s, _), _) -> do
          NSB.sendAll s $ msgToByteString msg
          bs <- NSB.recv s 4096
          case decodeMsg bs of
            Right m -> do
              putMVar client clientInfo
              pure $ Right m
            Left _ -> do
              putMVar client (clientInfo {_status = CnxClosed})
              pure $ Left msg

{-|
    Send a list of Riemann events

    Host and Time will be added if they do not exist on the Event
-}
sendEvents :: TCPConnection -> Seq PE.Event -> IO ()
sendEvents connection events = do
  eventsWithDefaults <- Event.withDefaults events
  result <-
    sendMsg connection $ P'.defaultValue {Msg.events = eventsWithDefaults}
  case result of
    Left msg -> hPutStrLn stderr $ "failed to send" ++ show msg
    Right _ -> pure ()
