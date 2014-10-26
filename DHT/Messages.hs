module DHT.Messages where 

import qualified DHT.Node as N
import DHT.Bencode

import qualified Data.Word as W
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

type NodeID = N.Word160
type Infohash = N.Word160
type Port = W.Word16
type Token = B.ByteString
type Peer = B.ByteString

data Query =
    -- All queries start with a transaction id and originating node id
    PingQ B.ByteString NodeID
  | FindNodeQ B.ByteString NodeID NodeID
  | GetPeersQ B.ByteString NodeID Infohash
  | AnnouncePeerQ B.ByteString NodeID Infohash Port Token

data Response =
    -- All responses start with a transaction id and originating node id
    PingR B.ByteString NodeID
  | FindNodeR B.ByteString NodeID [N.Node]
  | GetPeersPR B.ByteString NodeID Token [Peer]
  | GetPeersNR B.ByteString NodeID Token [N.Node]
  | AnnouncePeersR B.ByteString NodeID

type ErrorCode = Integer
type ErrorReason = B.ByteString

data Error = Error ErrorCode ErrorReason

encodeKVs :: [(BVal, BVal)] -> B.ByteString
encodeKVs = bencode . BDict . M.fromList

encodeQuery :: Query -> B.ByteString
encodeQuery (PingQ tid self) = encodeKVs [
    (BStr "id", BInt $ N.toInteger self)
  , (BStr "q", BStr "ping")
    ]
encodeQuery (FindNodeQ tid self targetNode) = encodeKVs [
    (BStr "id", BInt $ N.toInteger self)
  , (BStr "q", BStr "find_node")
  , (BStr "target", BInt . N.toInteger $ targetNode)
    ]
encodeQuery (GetPeersQ tid self targetInfohash) = encodeKVs [
    (BStr "id", BInt $ N.toInteger self)
  , (BStr "q", BStr "get_peers")
  , (BStr "info_hash", BInt . N.toInteger $ targetInfohash)
    ]
encodeQuery (AnnouncePeerQ tid self targetInfohash port token) = encodeKVs [
    (BStr "id", BInt $ N.toInteger self)
  , (BStr "q", BStr "announce_peer")
  , (BStr "info_hash", BInt . N.toInteger $ targetInfohash)
  , (BStr "port", BInt . toInteger $ port)
  , (BStr "token", BStr $ C.unpack token)
    ]
    
encodeResponse :: Response -> B.ByteString
encodeResponse (PingR tid from) = undefined
encodeResponse (FindNodeR tid from nodes) = undefined
encodeResponse (GetPeersPR tid from token peers) = undefined
encodeResponse (GetPeersNR tid from token nodes) = undefined
encodeResponse (AnnouncePeersR tid from) = undefined

encodeError :: Error -> B.ByteString
encodeError (Error code reason) = undefined
