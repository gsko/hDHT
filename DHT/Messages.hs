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
    PingQ NodeID
  | FindNodeQ NodeID NodeID
  | GetPeersQ NodeID Infohash
  | AnnouncePeerQ NodeID Infohash Port Token

data Response =
    PingR NodeID
  | FindNodeR NodeID [N.Node]
  -- TODO merge both of these messages
  | GetPeersPR NodeID Token [Peer]
  | GetPeersNR NodeID Token [N.Node]
  | AnnouncePeersR NodeID

type ErrorCode = Integer
type ErrorReason = B.ByteString

data Error = Error ErrorCode ErrorReason

encodeKVs :: [(BVal, BVal)] -> B.ByteString
encodeKVs = bencode . BDict . M.fromList

encodeQuery :: Query -> B.ByteString
encodeQuery (PingQ self) = encodeKVs [
    (BStr "id", BInt $ N.toInteger self)
  , (BStr "q", BStr "ping")
    ]
encodeQuery (FindNodeQ self targetNode) = encodeKVs [
    (BStr "id", BInt $ N.toInteger self)
  , (BStr "q", BStr "find_node")
  , (BStr "target", BInt . N.toInteger $ targetNode)
    ]
encodeQuery (GetPeersQ self targetInfohash) = encodeKVs [
    (BStr "id", BInt $ N.toInteger self)
  , (BStr "q", BStr "get_peers")
  , (BStr "info_hash", BInt . N.toInteger $ targetInfohash)
    ]
encodeQuery (AnnouncePeerQ self targetInfohash port token) = encodeKVs [
    (BStr "id", BInt $ N.toInteger self)
  , (BStr "q", BStr "announce_peer")
  , (BStr "info_hash", BInt . N.toInteger $ targetInfohash)
  , (BStr "port", BInt . toInteger $ port)
  , (BStr "token", BStr $ C.unpack token)
    ]
    

encodeResponse :: Response -> B.ByteString
encodeResponse (PingR from) = undefined
encodeResponse (FindNodeR from nodes) = undefined
encodeResponse (GetPeersPR from token peers) = undefined
encodeResponse (GetPeersNR from token nodes) = undefined
encodeResponse (AnnouncePeersR from) = undefined

encodeError :: Error -> B.ByteString
encodeError (Error code reason) = undefined


