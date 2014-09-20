module DHT.Messages where 

import Data.Word

import DHT.Node as D
import qualified Data.ByteString as B

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


