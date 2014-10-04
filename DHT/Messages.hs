module DHT.Messages where 

import Data.Word

import DHT.Node as N
import DHT.Bencode
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B

type NodeID = Word160
type Infohash = Word160
type Port = Word16
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

baseKVs :: Word160 -> [(BVal, BVal)]
baseKVs w = [(BStr "id", BInt $ N.toInteger w)]

encodeQuery :: Query -> B.ByteString
encodeQuery (PingQ self) = encodeKVs $ (baseKVs self) ++ [(BStr "q", BStr "ping")]
encodeQuery (FindNodeQ self targetNode) = encodeKVs $ (baseKVs self) ++ [
    (BStr "q", BStr "find_node"), (BStr "target", BInt . N.toInteger $ targetNode)]
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


