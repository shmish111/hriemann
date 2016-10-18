module Network.Monitoring.Riemann.TCP
    ( tcpClient
    , TCPClient
    , sendMsg
    , Port
    ) where

import           Control.Exception
import           Data.Binary.Put                      as Put
import qualified Data.ByteString.Lazy                 as BS
import qualified Data.ByteString.Lazy.Char8           as BC
import           Data.IORef
import qualified Data.Maybe                           as Maybe
import           Data.Text                            ( Text )
import qualified Network.Monitoring.Riemann.Proto.Msg as Msg
import           Network.Socket
import           Network.Socket.ByteString.Lazy       as NSB
import           Text.ProtocolBuffers.Get             as Get
import           Text.ProtocolBuffers.WireMessage     as WM

type ClientInfo = (HostName, Port, TCPStatus)

type TCPClient = IORef ClientInfo

data TCPStatus = CnxClosed
               | CnxOpen (Socket, AddrInfo)
    deriving (Show)

type Port = Int

-- | Try connecting with TCP to given host/port.
-- '''Note''': We never use IPv6 address resolved for given hostname.
tcpClient :: HostName -> Port -> IO TCPClient
tcpClient h p = do
    connection <- doConnect h p
    newIORef (h, p, CnxOpen connection)

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

sendMsg :: TCPClient -> Msg.Msg -> IO (Either Msg.Msg Msg.Msg)
sendMsg client msg = do
    clientInfo@(h, p, state) <- readIORef client
    (s, a) <- getConnection clientInfo
    sendAll s $ msgToByteString msg
    bs <- NSB.recv s 4096
    result <- pure $ decodeMsg bs
    case result of
        Right m -> return $ Right m
        Left e -> do
            writeIORef client (h, p, CnxClosed)
            return $ Left msg