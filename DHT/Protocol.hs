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

type NodeID = Word160
type Infohash = Word160
type Port = Word16
type Token = B.ByteString
type Peer = B.ByteString

data Query = PingQ NodeID
    | FindNodeQ NodeID NodeID
    | GetPeersQ NodeID Infohash
    | AnnouncePeerQ NodeID Infohash Port Token

data Response = PingR NodeID
    | FindNodeR NodeID [D.Node]
    -- TODO merge both of these messages
    | GetPeersPR NodeID Token [Peer]
    | GetPeersNR NodeID Token [D.Node]
    | AnnouncePeersR NodeID

type ErrorCode = Integer
type ErrorReason = B.ByteString

data Error = Error ErrorCode ErrorReason

encodeQuery :: Query -> B.ByteString
encodeQuery (PingQ self) = undefined
encodeQuery (FindNodeQ self targetNode) = undefined
encodeQuery (GetPeersQ self targetInfohash) = undefined
encodeQuery (AnnouncePeerQ self targetInfohash port token) = undefined

encodeResponse :: B.ByteString -> Response
encodeResponse msg = undefined
-- (PingR from)
-- (FindNodeR from nodes)
-- (GetPeersPR from token peers)
-- (GetPeersNR from token nodes)
-- (AnnouncePeersR from)

encodeError :: Error -> B.ByteString
encodeError (Error code reason) = undefined

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
