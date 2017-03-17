import DHT.Messages
import DHT.Node as N

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Data.List
import Text.Printf

type Address = (String, String)

sendMessage :: Socket -> Query -> SockAddr -> IO ()
sendMessage s q a = do
    let qstr = encodeQuery q
    sent <- sendTo s qstr a
    printf "%s <-out- (%u bytes) \"%s\"\n" (show a) sent (C.unpack qstr)

main :: IO()
main = do
    let pingQuery = "d1:ad2:id20:abcdefghij0123456789e1:q4:ping1:t2:aa1:y1:qe" :: String

    -- Prepare the socket
    addrInfos <- getAddrInfo Nothing (Just "weezy.us") (Just "1337")
    let addrInfo = head addrInfos
    let serverAddr = addrAddress addrInfo
    sock <- socket (addrFamily addrInfo) Datagram defaultProtocol
     
    -- Send the query
    let tid = "bobo" :: B.ByteString
    --sendMessage sock (PingQ tid $ N.fromInteger 0x7857627872756918275927593768395039686912) serverAddr
    let fn = GetPeersQ tid (N.fromInteger 0x009030915885103850983109) (N.fromInteger 0x0060938116830591)
    sendMessage sock fn serverAddr

    -- Receive the response
    (buf, recvAddr) <- recvFrom sock 1500
    printf "%s --in-> (%u bytes) %s\n" (show recvAddr) (B.length buf) (show buf)
