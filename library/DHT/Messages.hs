module DHT.Messages where 

import qualified DHT.Node as N
import DHT.Bencode

import qualified Data.Word as W
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Binary.Put as P

type NodeID = N.Word160
type Infohash = N.Word160
type Port = W.Word16
type Token = B.ByteString
type Peer = B.ByteString

data KRPC = KQuery Query | KResponse Response | KError Error

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

encodeWord160 :: N.Word160 -> P.Put
encodeWord160 (N.Word160 w1 w2 w3 w4 w5) = do
    P.putWord32be w1
    P.putWord32be w2
    P.putWord32be w3
    P.putWord32be w4
    P.putWord32be w5

intLog :: Int -> Int -> Int 
intLog 0 _ = 0 
intLog 1 _ = 0 
intLog n b = 1 + intLog (div n b) b

encodeKVs :: [(BVal, BVal)] -> B.ByteString
encodeKVs = bencode . BDict . M.fromList
encodeQuery :: Query -> B.ByteString
encodeQuery (PingQ tid self) = encodeKVs [
    (BStr "y", BStr "q")
  , (BStr "a", BDict $ M.fromList [
        (BStr "id", BStr . N.toBS $ self)
        ])
  , (BStr "t", BStr tid)
  , (BStr "q", BStr "ping")
    ]
encodeQuery (FindNodeQ tid self targetNode) = encodeKVs [
    (BStr "y", BStr "q")
  , (BStr "t", BStr tid)
  , (BStr "q", BStr "find_node")
  , (BStr "a", BDict $ M.fromList [
        (BStr "id", BStr . N.toBS $ self)
      , (BStr "target", BStr . N.toBS $ targetNode)
      ])
    ]
encodeQuery (GetPeersQ tid self targetInfohash) = encodeKVs [
    (BStr "y", BStr "q")
  , (BStr "q", BStr "get_peers")
  , (BStr "t", BStr tid)
  , (BStr "a", BDict $ M.fromList [
        (BStr "id", BStr . N.toBS $ self)
      , (BStr "info_hash", BStr . N.toBS $ targetInfohash)
      ])
    ]
encodeQuery (AnnouncePeerQ tid self targetInfohash port token) = encodeKVs [
    (BStr "y", BStr "q")
  , (BStr "t", BStr tid)
  , (BStr "q", BStr "announce_peer")
  , (BStr "a", BDict $ M.fromList [
        (BStr "id", BStr . N.toBS $ self)
      , (BStr "info_hash", BInt . N.toInteger $ targetInfohash)
      , (BStr "port", BInt . toInteger $ port)
      , (BStr "token", BStr token)
      ])
    ]
    
encodeResponse :: Response -> B.ByteString
encodeResponse (PingR tid from) = undefined
encodeResponse (FindNodeR tid from nodes) = undefined
encodeResponse (GetPeersPR tid from token peers) = undefined
encodeResponse (GetPeersNR tid from token nodes) = undefined
encodeResponse (AnnouncePeersR tid from) = undefined

encodeError :: Error -> B.ByteString
encodeError (Error code reason) = undefined

encodeKRPC :: KRPC -> B.ByteString
encodeKRPC (KQuery q) = encodeQuery q
encodeKRPC (KResponse r) = encodeResponse r
encodeKRPC (KError e) = encodeError e
