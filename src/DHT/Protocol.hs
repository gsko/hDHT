module DHT.Protocol where

import Network.Socket
import Data.List
import Data.Word
import Text.Printf

import DHT.Node as D
import qualified Data.ByteString as B

pingQuery :: String
--pingQuery = "d1:ad2:id20:1111111111111111111111:q4:ping1:t2:aa1:y1:qe"
pingQuery = "d1:ad2:id20:abcdefghij0123456789e1:q4:ping1:t2:aa1:y1:qe"

data Query = Ping Word160
    | FindNode Word160 Word160
    | GetPeers Word160 Word160
    | AnnouncePeer Word160 Word160 Word16 B.ByteString

main :: IO()
main = do
    -- Prepare the socket
    addrInfos <- getAddrInfo Nothing (Just "weezy.us") (Just "1337")
    let addrInfo = head addrInfos
    let serverAddr = addrAddress addrInfo
    sock <- socket (addrFamily addrInfo) Datagram defaultProtocol
     
    -- Send the query
    sent <- sendTo sock pingQuery serverAddr
    printf "%s <-out- (%u bytes) \"%s\"\n" (show serverAddr) sent pingQuery 

    -- Receive the response
    (buf, bufLen, recvAddr) <- recvFrom sock 1024
    printf "%s --in-> (%u bytes) %s\n" (show recvAddr) bufLen (show buf)
