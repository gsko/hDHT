module MessagesTest (tests) where

import DHT.Messages
import qualified DHT.Node as N

import Test.Framework
import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

tests = TestGroup "MessagesTest" [
    testCase "encodeQuery - PingQ" test_encodeQuery_ping
  , testCase "encodeQuery - FindNodeQ" test_encodeQuery_find_node
    ]

nodeID,infohash :: N.Word160
nodeID = N.fromInteger 5
infohash = N.fromInteger 10

test_encodeQuery_ping :: Assertion
test_encodeQuery_ping = "d2:idi5e1:q4:pinge" @=? encodeQuery (PingQ nodeID)

test_encodeQuery_find_node :: Assertion
test_encodeQuery_find_node = "d2:idi5e1:q9:find_node6:targeti10ee" @=? encodeQuery (FindNodeQ nodeID infohash)
