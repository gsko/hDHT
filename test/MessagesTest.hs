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
    testCase "encodeQuery - Ping" test_encodeQuery_ping
    ]

test_encodeQuery_ping :: Assertion
test_encodeQuery_ping = encodeQuery (PingQ $ N.fromInteger 5) @?= "d2:idi5e1:q4:pinge"
