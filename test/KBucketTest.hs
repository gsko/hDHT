module KBucketTest (tests) where
import DHT.KBucket as K
import DHT.Node as N

import Test.Framework
import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

x :: KBucket
x = KBucket nodes (N.fromInteger 0) (N.fromInteger $ 2^160 - 1) 256
    where nodes :: [N.Node]
          nodes = map makeNode [1..20]

makeNode :: Integer -> N.Node
makeNode = (N.Node . N.fromInteger)

tests = TestGroup "KBucketTest" [
    testCase "test: offernode takes nodes"
        (False @?= ((null . nodes) $ offernode (K.empty 256) (makeNode 5)))
    ]
