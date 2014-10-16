module KBucketTest where
import DHT.KBucket as K
import DHT.Node as N

import Test.Framework
import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

x = kbucket nodes (N.fromInteger 0) (N.fromInteger $ 2^160 - 1) 256
    where nodes :: [N.Node]
          nodes = map makeNode [1..20]

makeNode :: Integer -> N.Node
makeNode = N.Node . N.fromInteger

tests = TestGroup "KBucketTest" [
    testCase "test: offernode takes nodes" $
        let node = makeNode 5
            kbucket = K.empty 256
            kbucket' = offernode kbucket node in
        False @=? (null . nodes) kbucket'
  , testCase "test: offernode maintains node uniqueness" $
        let node = makeNode $ 2^120
            kbucket = K.empty 256
            kbucket' = offernode kbucket node
            kbucket'' = offernode kbucket' node in
        [node] @=? nodes kbucket''
  , testCase "test: removenode removes node" $
        let node = makeNode $ 2^150
            kbucket = offernode (K.empty 200) node
            kbucket' = removenode kbucket node in
        [] @=? nodes kbucket'
    ]
