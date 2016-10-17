module Network.Monitoring.Riemann.TCP
    ( tcpConnect
    , sendMsg
    , TCPState(..)
    , Port
    ) where

import           Control.Exception
import           Data.Binary.Put                      as Put
import qualified Data.ByteString.Lazy.Char8           as BC
import           Data.Text                            (Text)
import qualified Network.Monitoring.Riemann.Proto.Msg as Msg
import           Network.Socket
import           Network.Socket.ByteString.Lazy
import           Text.ProtocolBuffers.WireMessage     as WM

data TCPState = CnxClosed
              | CnxOpen (Socket, AddrInfo)
              | CnxError IOException
    deriving (Show)

type Port = Int

data MsgState = Ok
              | Error Text
              | Unknown

-- | Try connecting with TCP to given host/port.
-- '''Note''': We never use IPv6 address resolved for given hostname.
tcpConnect :: HostName -> Port -> IO TCPState
tcpConnect hn po = do
    putStrLn "reconnect"
    res <- try $ doConnect hn po
    return $
        case res of
            Left e  -> CnxError e
            Right s -> CnxOpen s

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

sendMsg :: TCPState -> Msg.Msg -> IO ()
sendMsg tcpState msg = case tcpState of
    CnxClosed      -> print "closed"
    CnxError e     -> print e
    CnxOpen (s, a) -> sendAll s $ msgToByteString msg
