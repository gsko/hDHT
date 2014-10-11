module MessagesTest (tests) where

import DHT.Messages
import qualified DHT.Node as N

import Test.Framework
import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Word as W

tests = TestGroup "MessagesTest" [
    testCase "encodeQuery - PingQ" test_encodeQuery_ping
  , testCase "encodeQuery - FindNodeQ" test_encodeQuery_find_node
  , testCase "encodeQuery - GetPeersQ" test_encodeQuery_get_peers
  , testCase "encodeQuery - AnnouncePeerQ" test_encodeQuery_announce_peer
    ]

nodeID,infohash :: N.Word160
nodeID = N.fromInteger 5
infohash = N.fromInteger 10

port :: W.Word16
port = 555

token :: B.ByteString
token = C.pack "token here"

test_encodeQuery_ping :: Assertion
test_encodeQuery_ping = "d2:idi5e1:q4:pinge" @=? encodeQuery (PingQ nodeID)

test_encodeQuery_find_node :: Assertion
test_encodeQuery_find_node = "d2:idi5e1:q9:find_node6:targeti10ee" @=?
    encodeQuery (FindNodeQ nodeID infohash)

test_encodeQuery_get_peers :: Assertion
test_encodeQuery_get_peers = "d2:idi5e9:info_hashi10e1:q9:get_peerse" @=?
    encodeQuery (GetPeersQ nodeID infohash)

test_encodeQuery_announce_peer :: Assertion
test_encodeQuery_announce_peer = 
    "d2:idi5e9:info_hashi10e4:porti555e1:q13:announce_peer5:token10:token heree" @=?
    encodeQuery (AnnouncePeerQ nodeID infohash port token)
